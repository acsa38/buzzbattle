# =====================================================================
#  BUZZ BATTLE 🔥 : compétition d'influence en ligne
#  ESC Clermont, Master Marketing | Data sciences appliquées au marketing
# ---------------------------------------------------------------------
#  Principe
#   1. Chaque étudiant (ou équipe) poste SA phrase buzz pour le brief.
#      Pendant qu'il tape, un "Buzzomètre IA" (NLP + GLM logistique
#      entraîné sur un historique de posts) estime son potentiel viral.
#   2. La classe vote en direct avec des jetons 🔥 (chrono possible).
#   3. Classement, course des votes, mur de buzz, mots de la classe...
#      tout se met à jour en temps réel : à projeter au tableau.
#   4. Labo : on ré-estime les GLM (Poisson = nb de votes, logistique =
#      buzz oui/non) sur l'historique, sur les votes de la classe, ou
#      les deux, et on compare le prix du public au prix de l'IA.
#
#  LANCER EN CLASSE (tout le monde sur le même wifi)
#    shiny::runApp("App_BuzzBattle.R", host = "0.0.0.0", port = 8080)
#    -> les étudiants ouvrent http://<IP_de_ton_PC>:8080 sur leur téléphone
#       (IP : `ipconfig` sous Windows, Réglages > Wi-Fi > Détails sous Mac)
#    Astuce : si le wifi de l'école isole les appareils, déploie sur
#    shinyapps.io avec UN SEUL processus R (Settings > Advanced :
#    Max Worker Processes = 1, Instances max = 1), sinon les votes
#    ne seraient pas partagés entre tous les étudiants.
#
#  Packages : install.packages(c("shiny","bslib","dplyr","ggplot2","plotly","DT"))
#  Code animateur par défaut : ESC2026 (modifiable ci-dessous)
# =====================================================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- 0. Paramètres animateur ---------------------------------------
ADMIN_CODE     <- "ESC2026"
BRAND          <- "FLARE"     # nom de marque (exclu du comptage des CAPITALES)
JETONS_DEFAUT  <- 3           # nb de 🔥 par votant
MAX_PHRASES    <- 1           # nb de phrases max par étudiant
MAX_CARACTERES <- 140
SAVE_FILE      <- "buzzbattle_sauvegarde.rds"   # sauvegarde auto (reprise après crash)
BRIEF_DEFAUT   <- paste0(
  "La marque FLARE lance la FLARE Run, sa nouvelle sneaker urbaine. ",
  "Écris LA phrase (", MAX_CARACTERES, " caractères max) qui fera exploser les réseaux sociaux."
)

# =====================================================================
# 1. MOTEUR NLP (fait maison, sans dépendance : lexiques français)
# =====================================================================
sans_accents <- function(x) chartr("àâäáãéèêëíìîïóòôöõúùûüçñœ",
                                   "aaaaaeeeeiiiiooooouuuucno", x)

LEX <- list(
  interpellation = c("tu","toi","ton","ta","tes","te","t","vous","votre","vos",
                     "viens","venez","rejoins","rejoignez","imagine","imaginez"),
  puissants = c("gratuit","gratuite","offert","offerte","nouveau","nouvelle","exclusif",
                "exclusive","secret","secrete","incroyable","enfin","revolution",
                "revolutionnaire","ultime","unique","inedit","inedite","jamais","meilleur",
                "meilleure","culte","legende","legendaire","viral","waouh","wow","dingue",
                "fou","folle","magique","osez","ose","impossible","record","breaking"),
  urgence = c("maintenant","vite","aujourd hui","ce soir","derniere chance","limite",
              "limitee","stock","immediatement","now","24h","48h","depeche","depechez",
              "seulement","bientot","urgent","demain","derniers","dernieres"),
  positif = c("bien","bon","bonne","beau","belle","super","top","genial","geniale",
              "parfait","parfaite","heureux","heureuse","fier","fiere","plaisir","adore",
              "adorer","aime","aimer","love","style","confort","libre","liberte","energie",
              "fort","forte","rapide","leger","legere","reve","victoire","gagne","gagner",
              "sourire","souris","joie","cool","incroyable","mieux","magique","fun"),
  negatif = c("mal","nul","nulle","triste","peur","fatigue","fatigues","douleur","ennui",
              "ennuyeux","pire","rate","perdu","perdre","lent","lourd","lourde","cher",
              "chere","probleme","stress","marre","ampoules","galere")
)

STOPWORDS <- c("le","la","les","un","une","des","de","du","d","l","et","ou","a","au","aux",
               "en","dans","sur","pour","par","avec","qui","que","quoi","ce","cet","cette",
               "ces","est","sont","on","il","elle","ils","elles","je","j","tu","te","t","toi",
               "ton","ta","tes","vous","votre","vos","nous","notre","nos","se","s","sa","son",
               "ses","ne","n","pas","plus","mais","comme","y","c","qu","m","me","mon","ma",
               "mes","si","tout","tous","the","to","va","vont","etre","fait")

# Variables NLP utilisées par les modèles (nom -> libellé lisible)
FEATURES <- c(
  n_mots         = "Longueur (nb de mots)",
  exclam         = "Points d'exclamation",
  question       = "Question posée",
  emoji          = "Emojis",
  interpellation = "Tutoiement / interpellation",
  mots_puissants = "Mots « puissants »",
  urgence        = "Urgence / rareté",
  sentiment      = "Sentiment (positif - négatif)",
  chiffres       = "Chiffres",
  majuscules     = "MOTS EN CAPITALES",
  hashtag        = "Hashtag",
  allit          = "Allitération / rythme"
)

tokeniser <- function(x) {
  t <- unlist(strsplit(x, "[^\\p{L}\\p{N}]+", perl = TRUE))
  t[nchar(t) > 0]
}

# Une phrase -> une ligne de variables NLP
nlp_une_phrase <- function(x) {
  x    <- enc2utf8(as.character(x))
  brut <- gsub("[’']", " ", x)
  low  <- sans_accents(tolower(brut))
  tok      <- tokeniser(low)
  tok_orig <- tokeniser(brut)
  low_esp  <- paste0(" ", paste(tok, collapse = " "), " ")

  compte <- function(lex) {
    mono  <- lex[!grepl(" ", lex)]
    multi <- lex[grepl(" ", lex)]
    sum(tok %in% mono) +
      sum(vapply(multi, function(l) grepl(paste0(" ", l, " "), low_esp, fixed = TRUE), logical(1)))
  }

  cp <- suppressWarnings(utf8ToInt(x)); if (anyNA(cp)) cp <- integer(0)
  n_emoji <- sum(cp >= 0x1F300 | (cp >= 0x2600 & cp <= 0x27BF))

  caps <- tok_orig[nchar(tok_orig) >= 2 &
                     grepl("^\\p{Lu}+$", tok_orig, perl = TRUE) &
                     toupper(tok_orig) != toupper(BRAND)]

  initiales <- substr(tok[nchar(tok) >= 3 & !tok %in% STOPWORDS], 1, 1)
  allit <- as.integer(length(initiales) >= 3 && max(table(initiales)) >= 3)

  data.frame(
    n_mots         = length(tok),
    n_car          = nchar(x),
    exclam         = min(lengths(regmatches(x, gregexpr("!", x, fixed = TRUE))), 3),
    question       = as.integer(grepl("?", x, fixed = TRUE)),
    emoji          = min(n_emoji, 3),
    interpellation = as.integer(compte(LEX$interpellation) > 0),
    mots_puissants = min(compte(LEX$puissants), 3),
    urgence        = min(compte(LEX$urgence), 3),
    sentiment      = compte(LEX$positif) - compte(LEX$negatif),
    chiffres       = as.integer(grepl("[0-9]", x)),
    majuscules     = min(length(caps), 3),
    hashtag        = as.integer(grepl("#\\p{L}", x, perl = TRUE)),
    allit          = allit
  )
}

nlp_features <- function(textes) {
  if (length(textes) == 0) return(nlp_une_phrase("")[0, ])
  do.call(rbind, lapply(textes, nlp_une_phrase))
}

# =====================================================================
# 2. HISTORIQUE SIMULÉ : 160 posts passés de la marque (données FICTIVES)
#    -> sert à entraîner "l'IA" du Buzzomètre avant même le 1er vote
# =====================================================================
cap_first <- function(s) paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))

generer_historique <- function(n = 160, seed = 2026) {
  set.seed(seed)
  ouvertures <- c("", "", "", "Enfin,", "Nouveau :", "Exclusif :", "Toi aussi,", "Attention :",
                  "Le secret :", "STOP.", "Ce soir,", "Vous allez adorer :", "Breaking :")
  coeurs <- c("tes pieds méritent mieux", "la sneaker qui fait parler toute la ville",
              "on a réinventé la course urbaine", "chaque pas compte",
              "marche plus loin, cours plus fort", "le confort rencontre enfin le style",
              "tes baskets vont être jalouses", "la FLARE Run est arrivée",
              "cours comme personne", "ta ville devient ton terrain de jeu",
              "une chaussure, mille trajets", "le bitume n'a qu'à bien se tenir",
              "du style sans compromis", "nous fabriquons des chaussures de sport",
              "produit disponible en plusieurs tailles", "une gamme de chaussures pour adultes",
              "légère comme une plume, solide comme un roc", "le secret des runners urbains",
              "fini les ampoules et la douleur", "cours, souris, recommence",
              "la liberté à chaque foulée", "rapide, rebelle, redoutable")
  fins <- c("", "", ".", "!", "!!", " 🔥", " 👟🔥", " #FLARE", " #RunTheCity", " : maintenant.",
            ", stock limité.", ". Dispo dès aujourd'hui !", " ?", ". 100 % recyclée.",
            " : 48h seulement !", ". Livraison offerte.")
  k <- n * 3
  textes <- trimws(paste(sample(ouvertures, k, TRUE),
                         paste0(sample(coeurs, k, TRUE), sample(fins, k, TRUE))))
  textes <- unique(cap_first(textes))[seq_len(n)]
  f <- nlp_features(textes)
  # "Vraie" mécanique cachée du buzz (les étudiants doivent la retrouver !)
  eta <- 0.9 + 0.30 * f$exclam + 0.30 * f$emoji + 0.50 * f$interpellation +
    0.35 * f$mots_puissants + 0.30 * f$urgence + 0.20 * f$sentiment - 0.06 * f$n_mots +
    0.25 * f$hashtag + 0.15 * f$chiffres - 0.20 * f$majuscules + 0.10 * f$question +
    0.30 * f$allit
  votes <- rnbinom(n, mu = exp(eta), size = 5)
  data.frame(id = seq_len(n), texte = textes, f, votes = votes,
             source = "Historique simulé", stringsAsFactors = FALSE) |>
    mutate(buzz = as.integer(votes >= quantile(votes, 0.75)))
}

HISTORIQUE <- generer_historique()

# =====================================================================
# 3. GLM : fonctions de modélisation
# =====================================================================
ajuster_glm <- function(df, vars, famille = c("poisson", "binomial"), avec_source = FALSE) {
  famille <- match.arg(famille)
  y <- if (famille == "poisson") "votes" else "buzz"
  if (is.null(df) || nrow(df) < 6 || length(unique(df[[y]])) < 2) return(NULL)
  vars <- vars[vapply(vars, function(v) length(unique(df[[v]])) > 1, logical(1))]
  if (avec_source && length(unique(df$source)) > 1) vars <- c(vars, "source")
  if (length(vars) == 0) vars <- "1"
  f <- as.formula(paste(y, "~", paste(vars, collapse = " + ")))
  tryCatch(suppressWarnings(glm(f, data = df,
                                family = if (famille == "poisson") poisson() else binomial())),
           error = function(e) NULL)
}

table_effets <- function(m) {
  co <- summary(m)$coefficients
  co <- co[rownames(co) != "(Intercept)", , drop = FALSE]
  if (nrow(co) == 0) return(NULL)
  ci <- suppressMessages(confint.default(m))[rownames(co), , drop = FALSE]
  lib <- ifelse(rownames(co) %in% names(FEATURES), FEATURES[rownames(co)],
                ifelse(grepl("^source", rownames(co)), "Effet « classe » vs historique",
                       rownames(co)))
  data.frame(var = rownames(co), libelle = lib, est = co[, 1], p = co[, 4],
             pct = (exp(co[, 1]) - 1) * 100,
             lo = (exp(ci[, 1]) - 1) * 100, hi = (exp(ci[, 2]) - 1) * 100,
             stringsAsFactors = FALSE)
}

# Le modèle "IA" du Buzzomètre : logistique entraînée sur l'historique
MODELE_IA <- ajuster_glm(HISTORIQUE, names(FEATURES), "binomial")
score_ia  <- function(f) as.numeric(predict(MODELE_IA, newdata = f, type = "response"))

CONSEILS <- c(interpellation = "Interpelle ton audience (tu / vous)",
              emoji = "Un emoji bien choisi ?",
              mots_puissants = "Un mot fort (nouveau, enfin, secret...)",
              urgence = "Crée de l'urgence (maintenant, 48h...)",
              exclam = "Un point d'exclamation ?",
              hashtag = "Un hashtag de marque #",
              allit = "Joue sur le rythme (allitération)")

# =====================================================================
# 4. ÉTAT PARTAGÉ ENTRE TOUS LES ÉTUDIANTS (même processus R)
# =====================================================================
etat_vide <- function() list(
  phrases = cbind(
    data.frame(id = integer(), pseudo = character(), equipe = character(),
               texte = character(), auteur = character(), heure = Sys.time()[0],
               score_ia = numeric(), stringsAsFactors = FALSE),
    nlp_features(character(0))),
  votes = data.frame(voter_id = character(), phrase_id = integer(),
                     heure = Sys.time()[0], stringsAsFactors = FALSE),
  depot_ouvert = TRUE, vote_ouvert = FALSE, reveal = FALSE,
  deadline = NULL, brief = BRIEF_DEFAUT, jetons = JETONS_DEFAUT
)

etat_initial <- if (file.exists(SAVE_FILE)) {
  tryCatch(readRDS(SAVE_FILE), error = function(e) etat_vide())
} else etat_vide()

BUZZ <- do.call(reactiveValues, etat_initial)

sauvegarder <- function() {
  e <- isolate(reactiveValuesToList(BUZZ))
  tmp <- paste0(SAVE_FILE, ".tmp")
  try({ saveRDS(e, tmp); file.rename(tmp, SAVE_FILE) }, silent = TRUE)
}

vote_actif <- function() {
  isTRUE(BUZZ$vote_ouvert) && (is.null(BUZZ$deadline) || Sys.time() < BUZZ$deadline)
}

tronque <- function(x, n) ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "…"), x)

layout_sombre <- function(p, ...) {
  p |>
    layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
           font = list(color = "#f4f4f8"), margin = list(l = 10, r = 20, t = 30, b = 40), ...) |>
    config(displayModeBar = FALSE)
}

# =====================================================================
# 5. UI
# =====================================================================
theme <- bs_theme(
  version = 5, bg = "#0d0f1f", fg = "#f4f4f8",
  primary = "#ff3d71", secondary = "#7c5cff", success = "#00d68f", warning = "#ffaa00",
  base_font = font_collection("Inter", "Segoe UI", "system-ui", "sans-serif")
)

css <- "
.card{background:#151834;border:1px solid #262a52;border-radius:18px}
.buzz-card{background:linear-gradient(135deg,#1b1e3a,#26214d);border:1px solid #30346a;
  border-radius:18px;padding:16px;height:100%;transition:transform .15s}
.buzz-card:hover{transform:translateY(-3px)}
.buzz-text{font-size:1.15rem;font-weight:650;line-height:1.35}
.btn-fire{border-radius:999px;font-weight:700}
.btn-like{color:#fff !important;border:1px solid #ff3d71;background:transparent}
.btn-like:hover{background:#ff3d71}
.chip{display:inline-block;padding:4px 11px;border-radius:999px;margin:3px;font-size:.85rem}
.chip-pos{background:rgba(0,214,143,.15);color:#3ff0b0}
.chip-neg{background:rgba(255,61,113,.15);color:#ff7a9c}
.chip-tip{background:rgba(124,92,255,.18);color:#c3b5ff}
.chrono{font-size:1.7rem;font-weight:800;color:#ffaa00}
.winner{font-size:1.6rem;font-weight:800;text-align:center;padding:22px;border-radius:20px;
  margin-bottom:12px;background:linear-gradient(90deg,#ff3d71,#7c5cff);animation:pulse 1.6s infinite}
.winner.ia{background:linear-gradient(90deg,#00a3ff,#00d68f)}
@keyframes pulse{0%{transform:scale(1)}50%{transform:scale(1.02)}100%{transform:scale(1)}}
.mur{max-height:360px;overflow-y:auto;display:flex;flex-wrap:wrap;gap:10px;align-items:center}
.mur-item{background:#1f2347;border-radius:14px;padding:8px 14px;font-weight:700}
.jeton{font-size:1.6rem;margin-right:4px}
.jeton.off{filter:grayscale(1);opacity:.25}
.verdict{font-size:1.3rem;font-weight:800;text-align:center}
"

js_voter <- "
$(document).on('shiny:connected', function(){
  var id = null;
  try { id = localStorage.getItem('buzzbattle_voter'); } catch(e) {}
  if(!id){ id = 'v' + Math.random().toString(36).slice(2, 11);
           try { localStorage.setItem('buzzbattle_voter', id); } catch(e) {} }
  Shiny.setInputValue('voter_id', id);
});
"

code_r <- '# ---- Rejouer l\'analyse sur le dataset exporté de la classe ----
classe <- read.csv2("buzzbattle_classe.csv", encoding = "UTF-8")

# 1) GLM de Poisson : qu\'est-ce qui fait monter le NOMBRE de votes ?
m_pois <- glm(votes ~ n_mots + exclam + emoji + interpellation +
                mots_puissants + urgence + sentiment + hashtag,
              data = classe, family = poisson)
summary(m_pois)
round((exp(coef(m_pois)) - 1) * 100, 1)   # effet en % sur le nb de votes attendu

# 2) Surdispersion ? (si >> 1 : quasipoisson ou binomiale négative)
sum(residuals(m_pois, type = "pearson")^2) / df.residual(m_pois)
m_qp <- update(m_pois, family = quasipoisson)
# m_nb <- MASS::glm.nb(formula(m_pois), data = classe)

# 3) GLM logistique : buzz (top 25 %) oui / non
m_logit <- glm(buzz ~ n_mots + exclam + emoji + interpellation +
                 mots_puissants + urgence + sentiment,
               data = classe, family = binomial)
exp(coef(m_logit))                          # odds ratios
1 - m_logit$deviance / m_logit$null.deviance  # pseudo-R2 de McFadden

# 4) L\'IA a-t-elle vu juste ? (score prédit avant vote vs votes réels)
cor(classe$score_ia, classe$votes, method = "spearman")

# 5) Bonus NLP : tester une nouvelle phrase avec la fonction de l\'appli
# source("App_BuzzBattle.R") puis :
# nlp_une_phrase("Toi aussi, cours comme personne 🔥 #FLARE")'

ui <- page_navbar(
  title = "🔥 Buzz Battle",
  theme = theme,
  fillable = FALSE,
  header = tags$head(tags$style(HTML(css)), tags$script(HTML(js_voter))),

  # ---- Mission ------------------------------------------------------
  nav_panel("🎯 Mission",
    layout_columns(col_widths = c(7, 5),
      card(card_header("📣 Le brief"),
           uiOutput("brief_ui"),
           hr(),
           h5("Comment ça marche"),
           tags$ol(
             tags$li(strong("Poster : "), "écris ta phrase buzz (", MAX_CARACTERES,
                     " caractères max). Le Buzzomètre IA te donne son pronostic en direct."),
             tags$li(strong("Voter : "), "tu as quelques jetons 🔥 à distribuer. ",
                     "Pas de vote pour ta propre phrase, 1 jeton max par phrase, tu peux retirer un vote."),
             tags$li(strong("Live : "), "classement, course des votes et mur de buzz au tableau."),
             tags$li(strong("Labo : "), "on ouvre le capot : variables NLP + GLM (Poisson et logistique).")
           ),
           h5("🏆 Deux trophées"),
           tags$ul(
             tags$li(strong("Prix du public"), " : la phrase qui récolte le plus de 🔥."),
             tags$li(strong("Prix de l'IA"), " : la phrase au meilleur score de Buzzomètre.")
           ),
           p(class = "text-muted", "Le public et l'IA seront-ils d'accord ? Réponse au débrief.")),
      card(card_header("🧠 Questions de débrief"),
           tags$ol(
             tags$li("Quelles variables NLP ont un effet significatif ? Lesquelles vous surprennent ?"),
             tags$li("Interprétez un coefficient du GLM de Poisson en % de votes attendus."),
             tags$li("L'IA, entraînée sur l'historique de la marque, a-t-elle prédit le choix de la classe ? Pourquoi ?"),
             tags$li("Que ne capte pas le modèle ? (humour, second degré, références, effet copain...)"),
             tags$li("Limites statistiques : petit n, votes non indépendants, surdispersion, lexique maison."),
             tags$li("Éthique : urgence artificielle, clickbait... où passe la frontière avec la manipulation ?")
           ),
           p(class = "text-muted small",
             "L'historique de 160 posts FLARE est entièrement simulé pour l'exercice."))
    )
  ),

  # ---- Poster -------------------------------------------------------
  nav_panel("✍️ Poster",
    layout_columns(col_widths = c(6, 6),
      card(card_header("Ta phrase buzz"),
           uiOutput("brief_court"),
           layout_columns(col_widths = c(6, 6),
             textInput("pseudo", "Pseudo", placeholder = "ex : Lea_M"),
             textInput("equipe", "Équipe (facultatif)", placeholder = "ex : Les Viraux")),
           textAreaInput("phrase", "Ta phrase", rows = 3, width = "100%",
                         placeholder = "Écris ici... le Buzzomètre réagit en direct 👉"),
           uiOutput("compteur"),
           actionButton("envoyer", "🚀 Publier ma phrase", class = "btn-primary btn-lg w-100"),
           br(), uiOutput("statut_depot")),
      card(card_header("🤖 Buzzomètre IA (NLP + GLM logistique)"),
           plotlyOutput("jauge", height = "230px", fill = FALSE),
           uiOutput("verdict"),
           uiOutput("chips"))
    )
  ),

  # ---- Voter --------------------------------------------------------
  nav_panel("🗳️ Voter",
    layout_columns(col_widths = c(6, 6),
      card(uiOutput("jetons_ui")),
      card(div(class = "text-center", uiOutput("chrono_vote")))),
    uiOutput("cartes")
  ),

  # ---- Live ---------------------------------------------------------
  nav_panel("📊 Live",
    uiOutput("palmares"),
    layout_columns(col_widths = c(3, 3, 3, 3),
      value_box(title = "✍️ Phrases", value = textOutput("vb_phrases"), theme = "primary"),
      value_box(title = "🙋 Votants", value = textOutput("vb_votants"), theme = "secondary"),
      value_box(title = "🔥 Votes", value = textOutput("vb_votes"), theme = "success"),
      card(div(class = "text-center pt-3", uiOutput("chrono_live")))),
    layout_columns(col_widths = c(7, 5),
      card(card_header("🏆 Classement en direct"), plotlyOutput("classement", height = "420px", fill = FALSE)),
      card(card_header("🏁 La course aux votes (top 5)"), plotlyOutput("course", height = "420px", fill = FALSE))),
    layout_columns(col_widths = c(4, 4, 4),
      card(card_header("🧱 Mur de buzz"), uiOutput("mur")),
      card(card_header("💬 Les mots de la classe"), plotlyOutput("mots", height = "340px", fill = FALSE)),
      card(card_header("🤖 L'IA avait-elle raison ?"), plotlyOutput("ia_vs_public", height = "340px", fill = FALSE)))
  ),

  # ---- Labo ---------------------------------------------------------
  nav_panel("🧠 Labo NLP & GLM",
    layout_sidebar(
      sidebar = sidebar(width = 300,
        radioButtons("donnees", "Données d'entraînement",
                     c("Historique simulé (160 posts)" = "histo",
                       "Votes de la classe (live)" = "live",
                       "Historique + classe" = "both"), selected = "histo"),
        radioButtons("famille", "Modèle GLM",
                     c("Poisson : nb de votes" = "poisson",
                       "Logistique : buzz oui / non" = "binomial")),
        checkboxGroupInput("vars", "Variables NLP",
                           choices = setNames(names(FEATURES), FEATURES),
                           selected = names(FEATURES))
      ),
      layout_columns(col_widths = c(7, 5),
        card(card_header("Effet de chaque variable (toutes choses égales par ailleurs)"),
             plotlyOutput("coefs", height = "430px", fill = FALSE)),
        card(card_header("Qualité & lecture du modèle"), uiOutput("metriques"))),
      layout_columns(col_widths = c(5, 7),
        card(card_header("summary(glm)"), verbatimTextOutput("resume")),
        card(card_header("Les données (variables NLP extraites)"), DTOutput("table_nlp")))
    )
  ),

  # ---- Code R -------------------------------------------------------
  nav_panel("💻 Code R",
    card(card_header("Refaire l'analyse dans RStudio (dataset exporté par l'animateur)"),
         tags$pre(style = "background:#0a0c18;color:#cfd3ff;padding:16px;border-radius:12px",
                  code_r))
  ),

  nav_spacer(),
  nav_panel("🔒 Animateur", uiOutput("admin_ui"))
)

# =====================================================================
# 6. SERVER
# =====================================================================
server <- function(input, output, session) {

  moi <- reactive({
    v <- input$voter_id
    if (is.null(v) || !nzchar(v)) session$token else v
  })
  admin_ok <- reactiveVal(FALSE)

  # Données live (phrases + votes + variable buzz)
  live_df <- reactive({
    ph <- BUZZ$phrases
    cnt <- BUZZ$votes |> count(phrase_id, name = "votes")
    d <- ph |> left_join(cnt, by = c("id" = "phrase_id")) |>
      mutate(votes = as.integer(coalesce(votes, 0L)), source = "Live (classe)")
    seuil <- if (nrow(d) > 0) max(1, quantile(d$votes, 0.75)) else 1
    d |> mutate(buzz = as.integer(votes >= seuil))
  })

  # ---- Brief --------------------------------------------------------
  output$brief_ui    <- renderUI(p(style = "font-size:1.25rem;font-weight:600", BUZZ$brief))
  output$brief_court <- renderUI(p(class = "text-muted", BUZZ$brief))

  # ---- Buzzomètre ---------------------------------------------------
  phrase_d <- debounce(reactive(input$phrase), 350)
  feat_saisie <- reactive({
    txt <- phrase_d()
    if (is.null(txt) || !nzchar(trimws(txt))) return(NULL)
    nlp_une_phrase(txt)
  })

  output$compteur <- renderUI({
    n <- nchar(input$phrase %||% "")
    col <- if (n > MAX_CARACTERES) "#ff3d71" else "#8a8fb5"
    p(style = paste0("color:", col, ";text-align:right;margin-top:-8px"),
      paste0(n, " / ", MAX_CARACTERES))
  })

  output$jauge <- renderPlotly({
    f <- feat_saisie()
    p <- if (is.null(f)) 0 else round(100 * score_ia(f))
    plot_ly(type = "indicator", mode = "gauge+number", value = p,
            number = list(suffix = " %", font = list(size = 44)),
            gauge = list(
              axis = list(range = c(0, 100), tickcolor = "#f4f4f8"),
              bar = list(color = if (p >= 60) "#ff3d71" else if (p >= 30) "#ffaa00" else "#7c5cff"),
              bgcolor = "#1b1e3a", borderwidth = 0,
              steps = list(list(range = c(0, 30), color = "#1d2040"),
                           list(range = c(30, 60), color = "#242852"),
                           list(range = c(60, 100), color = "#2d2160")))) |>
      layout_sombre(margin = list(l = 30, r = 30, t = 20, b = 0))
  })

  output$verdict <- renderUI({
    f <- feat_saisie()
    if (is.null(f)) return(p(class = "verdict text-muted", "Tape ta phrase ✍️"))
    s <- score_ia(f)
    txt <- if (s >= 0.6) "🔥 Potentiel viral !" else if (s >= 0.3) "🙂 Ça peut prendre..." else "😴 Flop probable"
    p(class = "verdict", txt)
  })

  output$chips <- renderUI({
    f <- feat_saisie(); if (is.null(f)) return(NULL)
    co <- coef(MODELE_IA); co <- co[names(co) %in% names(FEATURES) & !is.na(co)]
    contrib <- co * unlist(f[1, names(co)])
    pos <- names(sort(contrib[contrib > 0.01], decreasing = TRUE))
    neg <- names(sort(contrib[contrib < -0.01]))
    manque <- names(co)[co > 0 & unlist(f[1, names(co)]) == 0]
    manque <- head(intersect(manque, names(CONSEILS)), 3)
    tagList(
      if (length(pos)) div(strong("Ce qui booste : "),
                           lapply(pos, function(v) span(class = "chip chip-pos", "▲", FEATURES[[v]]))),
      if (length(neg)) div(strong("Ce qui freine : "),
                           lapply(neg, function(v) span(class = "chip chip-neg", "▼", FEATURES[[v]]))),
      if (length(manque)) div(strong("Pistes : "),
                              lapply(manque, function(v) span(class = "chip chip-tip", "💡", CONSEILS[[v]]))),
      p(class = "text-muted small mt-2",
        sprintf("%d mots · %d caractères · sentiment %+d", f$n_mots, f$n_car, f$sentiment))
    )
  })

  # ---- Dépôt d'une phrase -------------------------------------------
  output$statut_depot <- renderUI({
    mine <- BUZZ$phrases |> filter(auteur == moi())
    tagList(
      if (!BUZZ$depot_ouvert) div(class = "alert alert-warning mt-2", "⛔ Le dépôt des phrases est fermé."),
      if (nrow(mine)) div(class = "alert alert-success mt-2",
                          "✅ Ta phrase est en ligne : « ", mine$texte[nrow(mine)], " »")
    )
  })

  observeEvent(input$envoyer, {
    txt <- trimws(input$phrase %||% ""); ps <- trimws(input$pseudo %||% "")
    eq  <- trimws(input$equipe %||% "")
    alerte <- function(m) showNotification(m, type = "warning", duration = 4)
    if (!BUZZ$depot_ouvert) return(alerte("Le dépôt est fermé."))
    if (!nzchar(ps)) return(alerte("Indique un pseudo 🙂"))
    if (nchar(txt) < 5) return(alerte("Ta phrase est un peu courte..."))
    if (nchar(txt) > MAX_CARACTERES) return(alerte(paste("Maximum", MAX_CARACTERES, "caractères.")))
    ph <- BUZZ$phrases
    if (sum(ph$auteur == moi()) >= MAX_PHRASES)
      return(alerte(paste("Tu as déjà posté", MAX_PHRASES, "phrase(s).")))
    if (tolower(txt) %in% tolower(ph$texte)) return(alerte("Cette phrase existe déjà !"))
    f <- nlp_une_phrase(txt)
    nouvelle <- data.frame(id = as.integer(max(c(0L, ph$id)) + 1L), pseudo = ps,
                           equipe = ifelse(nzchar(eq), eq, "Solo"), texte = txt,
                           auteur = moi(), heure = Sys.time(), score_ia = score_ia(f),
                           f, stringsAsFactors = FALSE)
    BUZZ$phrases <- bind_rows(ph, nouvelle)
    sauvegarder()
    updateTextAreaInput(session, "phrase", value = "")
    showNotification("Phrase publiée 🚀 Direction l'onglet Voter !", type = "message")
  })

  # ---- Vote ---------------------------------------------------------
  chrono_ui <- function() {
    invalidateLater(1000)
    if (!isTRUE(BUZZ$vote_ouvert)) return(span(class = "badge bg-secondary fs-6", "🗳️ Vote fermé"))
    if (is.null(BUZZ$deadline)) return(span(class = "badge bg-success fs-6", "🗳️ Vote ouvert"))
    reste <- as.numeric(difftime(BUZZ$deadline, Sys.time(), units = "secs"))
    if (reste <= 0) return(span(class = "badge bg-danger fs-6", "⏱️ Temps écoulé !"))
    span(class = "chrono", sprintf("⏱️ %02d:%02d", floor(reste / 60), floor(reste %% 60)))
  }
  output$chrono_vote <- renderUI(chrono_ui())
  output$chrono_live <- renderUI(chrono_ui())

  output$jetons_ui <- renderUI({
    n_util <- sum(BUZZ$votes$voter_id == moi()); tot <- BUZZ$jetons
    div(strong("Tes jetons : "),
        lapply(seq_len(tot), function(i)
          span(class = paste("jeton", if (i <= tot - n_util) "" else "off"), "🔥")),
        span(class = "text-muted ms-2", sprintf("(%d / %d restants)", max(tot - n_util, 0), tot)))
  })

  output$cartes <- renderUI({
    ph <- BUZZ$phrases; vo <- BUZZ$votes; v <- moi()
    if (nrow(ph) == 0)
      return(div(class = "text-center text-muted p-5 fs-5", "Aucune phrase pour l'instant... à vos claviers ! ✍️"))
    mes_votes <- vo$phrase_id[vo$voter_id == v]
    graine <- sum(utf8ToInt(v))
    ph <- ph[order(((ph$id + graine) * 2654435761) %% 4294967296), ]   # ordre mélangé, propre à chacun
    ouvert <- vote_actif()
    cartes <- lapply(seq_len(nrow(ph)), function(i) {
      r <- ph[i, ]; deja <- r$id %in% mes_votes
      btn <- if (r$auteur == v) {
        span(class = "badge bg-secondary", "✍️ Ta phrase")
      } else {
        tags$button(
          class = paste("btn btn-fire", if (deja) "btn-success" else "btn-like"),
          disabled = if (!ouvert) NA else NULL,
          onclick = sprintf("Shiny.setInputValue('vote_click',{id:%d,n:Math.random()},{priority:'event'})", r$id),
          if (deja) "✅ Voté" else "🔥 Je like")
      }
      div(class = "buzz-card",
          p(class = "buzz-text", r$texte),
          div(class = "d-flex justify-content-between align-items-center",
              span(class = "text-muted small", paste0("#", r$id)), btn))
    })
    do.call(layout_column_wrap, c(list(width = "300px", heights_equal = "row"), cartes))
  })

  observeEvent(input$vote_click, {
    pid <- as.integer(input$vote_click$id); v <- moi()
    if (!vote_actif()) return(showNotification("Le vote n'est pas ouvert.", type = "warning"))
    vo <- BUZZ$votes; ph <- BUZZ$phrases
    deja <- vo$voter_id == v & vo$phrase_id == pid
    if (any(deja)) {
      BUZZ$votes <- vo[!deja, ]
    } else {
      if (any(ph$id == pid & ph$auteur == v))
        return(showNotification("Pas de vote pour ta propre phrase 😉", type = "warning"))
      if (sum(vo$voter_id == v) >= BUZZ$jetons)
        return(showNotification("Plus de jetons ! Retire un vote pour en récupérer un.", type = "warning"))
      BUZZ$votes <- bind_rows(vo, data.frame(voter_id = v, phrase_id = pid, heure = Sys.time(),
                                             stringsAsFactors = FALSE))
    }
    sauvegarder()
  })

  # ---- Live ---------------------------------------------------------
  output$vb_phrases <- renderText(nrow(BUZZ$phrases))
  output$vb_votants <- renderText(length(unique(BUZZ$votes$voter_id)))
  output$vb_votes   <- renderText(nrow(BUZZ$votes))

  output$classement <- renderPlotly({
    d <- live_df()
    validate(need(nrow(d) > 0, "En attente des premières phrases..."))
    rev_on <- isTRUE(BUZZ$reveal)
    d <- d |> arrange(desc(votes), id) |> head(10) |>
      mutate(lab = paste0("#", id, " · ", tronque(texte, 38),
                          if (rev_on) paste0("  (", pseudo, ")") else ""))
    d$lab <- factor(d$lab, levels = rev(d$lab))
    plot_ly(d, x = ~votes, y = ~lab, type = "bar", orientation = "h",
            text = ~votes, textposition = "outside", hovertext = ~texte, hoverinfo = "text",
            marker = list(color = ~votes, colorscale = list(c(0, "#7c5cff"), c(1, "#ff3d71")))) |>
      layout_sombre(xaxis = list(title = "🔥 votes", gridcolor = "#262a52"),
                    yaxis = list(title = ""))
  })

  output$course <- renderPlotly({
    vo <- BUZZ$votes; d <- live_df()
    validate(need(nrow(vo) > 0, "Le premier vote lancera la course 🏁"))
    top <- d |> arrange(desc(votes)) |> head(5)
    cour <- vo |> filter(phrase_id %in% top$id) |> arrange(heure) |>
      group_by(phrase_id) |> mutate(cumul = row_number()) |> ungroup() |>
      left_join(top |> select(id, texte), by = c("phrase_id" = "id")) |>
      mutate(lab = paste0("#", phrase_id, " ", tronque(texte, 22)))
    plot_ly(cour, x = ~heure, y = ~cumul, color = ~lab, type = "scatter",
            mode = "lines+markers", line = list(shape = "hv", width = 3),
            colors = c("#ff3d71", "#7c5cff", "#00d68f", "#ffaa00", "#00a3ff")) |>
      layout_sombre(xaxis = list(title = "", gridcolor = "#262a52"),
                    yaxis = list(title = "🔥 cumulés", gridcolor = "#262a52"),
                    legend = list(orientation = "h", y = -0.3, font = list(size = 11)))
  })

  output$mur <- renderUI({
    d <- live_df()
    if (nrow(d) == 0) return(p(class = "text-muted", "Le mur se remplira avec vos phrases."))
    d <- d |> arrange(desc(votes)) |> head(15)
    mx <- max(1, d$votes)
    div(class = "mur", lapply(seq_len(nrow(d)), function(i)
      span(class = "mur-item",
           style = sprintf("font-size:%.2frem;opacity:%.2f", 0.85 + 0.65 * d$votes[i] / mx,
                           0.55 + 0.45 * d$votes[i] / mx),
           d$texte[i])))
  })

  output$mots <- renderPlotly({
    d <- live_df()
    validate(need(nrow(d) > 0, "Pas encore de mots..."))
    tok <- unlist(lapply(d$texte, function(x) tokeniser(tolower(gsub("[’']", " ", x)))))
    tok <- tok[!sans_accents(tok) %in% STOPWORDS & nchar(tok) > 2]
    validate(need(length(tok) > 0, "Pas encore de mots..."))
    m <- as.data.frame(sort(table(tok), decreasing = TRUE)[1:min(12, length(unique(tok)))],
                       stringsAsFactors = FALSE)
    names(m) <- c("mot", "n")
    m$mot <- factor(m$mot, levels = rev(m$mot))
    plot_ly(m, x = ~n, y = ~mot, type = "bar", orientation = "h",
            marker = list(color = "#7c5cff")) |>
      layout_sombre(xaxis = list(title = "occurrences", gridcolor = "#262a52"),
                    yaxis = list(title = ""))
  })

  output$ia_vs_public <- renderPlotly({
    d <- live_df()
    validate(need(nrow(d) >= 3, "Il faut au moins 3 phrases pour comparer."))
    rho <- suppressWarnings(cor(d$score_ia, d$votes, method = "spearman"))
    plot_ly(d, x = ~score_ia * 100, y = ~votes, type = "scatter", mode = "markers",
            text = ~paste0("#", id, " ", texte), hoverinfo = "text",
            marker = list(size = 14, color = "#00d68f", line = list(color = "#fff", width = 1))) |>
      layout_sombre(
        title = list(text = sprintf("Corrélation de rang (Spearman) : %s",
                                    ifelse(is.na(rho), "n.d.", sprintf("%.2f", rho))),
                     font = list(size = 13)),
        xaxis = list(title = "Score Buzzomètre IA (%)", gridcolor = "#262a52"),
        yaxis = list(title = "🔥 votes de la classe", gridcolor = "#262a52"))
  })

  output$palmares <- renderUI({
    invalidateLater(2000)
    d <- live_df()
    if (!isTRUE(BUZZ$reveal) || vote_actif() || nrow(d) == 0 || nrow(BUZZ$votes) == 0) return(NULL)
    pub <- d |> arrange(desc(votes), desc(score_ia)) |> slice(1)
    ia  <- d |> arrange(desc(score_ia)) |> slice(1)
    layout_columns(col_widths = c(6, 6),
      div(class = "winner", "🏆 Prix du public", br(),
          span(style = "font-size:1.15rem", "« ", pub$texte, " »"), br(),
          span(style = "font-size:1rem", pub$pseudo, " · ", pub$equipe, " · ", pub$votes, " 🔥")),
      div(class = "winner ia", "🤖 Prix de l'IA", br(),
          span(style = "font-size:1.15rem", "« ", ia$texte, " »"), br(),
          span(style = "font-size:1rem", ia$pseudo, " · ", ia$equipe, " · ",
               round(100 * ia$score_ia), " % de potentiel")))
  })

  # ---- Labo NLP & GLM -----------------------------------------------
  donnees_labo <- reactive({
    cols <- c("texte", names(FEATURES), "votes", "buzz", "source")
    switch(input$donnees,
           histo = HISTORIQUE[, cols],
           live  = live_df()[, cols],
           both  = bind_rows(HISTORIQUE[, cols], live_df()[, cols]))
  })

  modele_labo <- reactive({
    req(input$vars)
    ajuster_glm(donnees_labo(), input$vars, input$famille, avec_source = input$donnees == "both")
  })

  output$coefs <- renderPlotly({
    m <- modele_labo()
    validate(need(!is.null(m), "Pas assez de données (ou pas assez de variation des votes) pour estimer le modèle. Il faut au moins 6 phrases votées."))
    tb <- table_effets(m)
    validate(need(!is.null(tb), "Aucune variable estimable."))
    tb <- tb |> arrange(pct) |> mutate(libelle = factor(libelle, levels = libelle),
      couleur = ifelse(p < 0.05, ifelse(est > 0, "#00d68f", "#ff3d71"), "#8a8fb5"),
      survol = sprintf("%s<br>Effet : %+.0f %%<br>IC95 : [%+.0f ; %+.0f] %%<br>p = %.3f",
                       libelle, pct, lo, hi, p))
    unite <- if (input$famille == "poisson") "Effet sur le nb de votes attendu (%)" else "Effet sur les chances de buzz (odds, %)"
    plot_ly(tb, x = ~pct, y = ~libelle, type = "scatter", mode = "markers",
            error_x = list(type = "data", symmetric = FALSE, array = ~hi - pct,
                           arrayminus = ~pct - lo, color = "#8a8fb5"),
            marker = list(size = 13, color = ~couleur),
            hovertext = ~survol, hoverinfo = "text") |>
      layout_sombre(xaxis = list(title = unite, zeroline = TRUE, zerolinecolor = "#ffffff",
                                 zerolinewidth = 2, gridcolor = "#262a52"),
                    yaxis = list(title = ""),
                    annotations = list(list(x = 1, y = 1.06, xref = "paper", yref = "paper",
                                            showarrow = FALSE, xanchor = "right",
                                            text = "<span style='color:#00d68f'>● booste (p<5%)</span>  <span style='color:#ff3d71'>● freine (p<5%)</span>  <span style='color:#8a8fb5'>● non significatif</span>",
                                            font = list(size = 11))))
  })

  output$metriques <- renderUI({
    m <- modele_labo(); if (is.null(m)) return(p(class = "text-muted", "Modèle non estimable pour l'instant."))
    r2 <- 1 - m$deviance / m$null.deviance
    tb <- table_effets(m)
    meilleur <- if (!is.null(tb)) tb |> filter(p < 0.05, var != "n_mots") |> arrange(desc(pct)) |> head(1)
    infos <- list(
      tags$li("Observations : ", strong(nrow(m$model))),
      tags$li("AIC : ", strong(round(AIC(m), 1)), span(class = "text-muted", " (plus bas = mieux, à comparer entre modèles)")),
      tags$li("Pseudo-R² de McFadden : ", strong(sprintf("%.2f", r2)))
    )
    if (input$famille == "poisson") {
      disp <- sum(residuals(m, type = "pearson")^2) / m$df.residual
      infos <- c(infos, list(tags$li("Dispersion : ", strong(sprintf("%.2f", disp)),
        if (disp > 1.5) span(class = "text-warning", " ⚠️ surdispersion : essayer quasipoisson / binomiale négative")
        else span(class = "text-muted", " (≈ 1 : OK pour Poisson)"))))
    } else {
      acc <- mean((fitted(m) > 0.5) == m$y)
      infos <- c(infos, list(tags$li("Taux de bonne classification (seuil 0,5) : ", strong(sprintf("%.0f %%", 100 * acc)))))
    }
    lecture <- if (input$famille == "poisson") {
      "Lecture : exp(β) - 1 = variation en % du nombre de votes attendu quand la variable augmente d'une unité, les autres restant fixes."
    } else {
      "Lecture : exp(β) - 1 = variation en % des chances (odds) de faire partie du top 25 %, les autres variables restant fixes."
    }
    phrase_cle <- if (!is.null(meilleur) && nrow(meilleur) == 1) {
      div(class = "alert alert-success mt-3", "💡 Dans ces données, ", strong(meilleur$libelle), " fait varier ",
          if (input$famille == "poisson") "le nombre de votes attendu" else "les chances de buzz",
          " de ", strong(sprintf("%+.0f %%", meilleur$pct)), " (p = ", sprintf("%.3f", meilleur$p), ").")
    } else div(class = "alert alert-secondary mt-3", "Aucune variable significative au seuil de 5 % : n trop petit ? Effet réel absent ?")
    tagList(tags$ul(infos), p(class = "text-muted", lecture), phrase_cle,
            if (input$donnees == "both") p(class = "small text-muted",
              "Le modèle « Historique + classe » ajoute une variable « source » (effet fixe) car les échelles de votes diffèrent."))
  })

  output$resume <- renderPrint({
    m <- modele_labo(); if (is.null(m)) return(cat("Modèle non estimable pour l'instant."))
    summary(m)
  })

  output$table_nlp <- renderDT({
    d <- donnees_labo()
    datatable(d |> select(texte, votes, buzz, all_of(names(FEATURES))),
              rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE, dom = "ftip"),
              class = "compact") |>
      formatStyle(columns = seq_len(ncol(d)), color = "#f4f4f8", backgroundColor = "#151834")
  })

  # ---- Animateur ----------------------------------------------------
  output$admin_ui <- renderUI({
    if (!admin_ok()) {
      return(card(style = "max-width:420px;margin:auto",
                  card_header("Espace animateur"),
                  passwordInput("code", "Code animateur"),
                  actionButton("login", "Entrer", class = "btn-primary")))
    }
    layout_columns(col_widths = c(4, 4, 4),
      card(card_header("⚙️ Phases du jeu"),
           checkboxInput("adm_depot", "Dépôt des phrases ouvert", isolate(BUZZ$depot_ouvert)),
           checkboxInput("adm_vote", "Vote ouvert (sans chrono)", isolate(BUZZ$vote_ouvert)),
           checkboxInput("adm_reveal", "Révéler auteurs + palmarès", isolate(BUZZ$reveal)),
           numericInput("adm_jetons", "Jetons 🔥 par votant", isolate(BUZZ$jetons), 1, 10),
           hr(),
           numericInput("adm_minutes", "Chrono de vote (minutes)", 3, 1, 30),
           actionButton("adm_chrono", "⏱️ Lancer le vote chronométré", class = "btn-primary w-100 mb-2"),
           actionButton("adm_stop", "⏹ Clore le vote", class = "btn-outline-light w-100"),
           p(class = "small text-muted mt-2", "Lancer le chrono ferme le dépôt et ouvre le vote.")),
      card(card_header("📣 Brief & modération"),
           textAreaInput("adm_brief", "Brief affiché", isolate(BUZZ$brief), rows = 5),
           actionButton("adm_maj_brief", "Mettre à jour le brief", class = "w-100"),
           hr(),
           numericInput("adm_suppr_id", "Supprimer la phrase n°", NA, min = 1),
           actionButton("adm_suppr", "🗑 Supprimer", class = "btn-outline-danger w-100"),
           hr(), DTOutput("adm_table")),
      card(card_header("💾 Données"),
           actionButton("adm_demo", "🧪 Injecter 12 phrases + votes de démo", class = "w-100 mb-2"),
           downloadButton("adm_export", "⬇️ Exporter le dataset de la classe (CSV)", class = "w-100 mb-2"),
           hr(),
           actionButton("adm_reset", "💣 Tout réinitialiser", class = "btn-danger w-100"),
           hr(),
           p(class = "small text-muted",
             "Sauvegarde automatique dans ", code(SAVE_FILE),
             " : si l'appli redémarre, la partie reprend où elle en était."))
    )
  })

  observeEvent(input$login, {
    if (identical(input$code, ADMIN_CODE)) admin_ok(TRUE)
    else showNotification("Code incorrect", type = "error")
  })

  observeEvent(input$adm_depot,  { BUZZ$depot_ouvert <- input$adm_depot; sauvegarder() }, ignoreInit = TRUE)
  observeEvent(input$adm_vote, {
    if (!identical(input$adm_vote, isTRUE(BUZZ$vote_ouvert))) {   # vrai changement manuel
      BUZZ$vote_ouvert <- input$adm_vote; BUZZ$deadline <- NULL; sauvegarder()
    }
  }, ignoreInit = TRUE)
  observeEvent(input$adm_reveal, { BUZZ$reveal <- input$adm_reveal; sauvegarder() }, ignoreInit = TRUE)
  observeEvent(input$adm_jetons, { req(input$adm_jetons >= 1); BUZZ$jetons <- as.integer(input$adm_jetons); sauvegarder() }, ignoreInit = TRUE)

  observeEvent(input$adm_chrono, {
    BUZZ$depot_ouvert <- FALSE; BUZZ$vote_ouvert <- TRUE; BUZZ$reveal <- FALSE
    BUZZ$deadline <- Sys.time() + 60 * (input$adm_minutes %||% 3)
    updateCheckboxInput(session, "adm_depot", value = FALSE)
    updateCheckboxInput(session, "adm_vote", value = TRUE)
    updateCheckboxInput(session, "adm_reveal", value = FALSE)
    sauvegarder(); showNotification("Vote lancé ⏱️", type = "message")
  })
  observeEvent(input$adm_stop, {
    BUZZ$vote_ouvert <- FALSE; BUZZ$deadline <- NULL
    updateCheckboxInput(session, "adm_vote", value = FALSE); sauvegarder()
  })
  observeEvent(input$adm_maj_brief, { BUZZ$brief <- input$adm_brief; sauvegarder() })

  output$adm_table <- renderDT({
    datatable(BUZZ$phrases |> select(id, pseudo, texte), rownames = FALSE, class = "compact",
              options = list(pageLength = 5, dom = "tp")) |>
      formatStyle(1:3, color = "#f4f4f8", backgroundColor = "#151834")
  })

  observeEvent(input$adm_suppr, {
    pid <- input$adm_suppr_id; req(!is.na(pid))
    BUZZ$phrases <- BUZZ$phrases |> filter(id != pid)
    BUZZ$votes   <- BUZZ$votes |> filter(phrase_id != pid)
    sauvegarder(); showNotification(paste("Phrase", pid, "supprimée"))
  })

  observeEvent(input$adm_demo, {
    ph <- BUZZ$phrases
    deja <- tolower(ph$texte)
    pool <- HISTORIQUE$texte[!tolower(HISTORIQUE$texte) %in% deja]
    txt <- sample(pool, min(12, length(pool)))
    f <- nlp_features(txt)
    ids <- max(c(0L, ph$id)) + seq_along(txt)
    noms <- c("Lea","Hugo","Ines","Tom","Chloe","Nael","Jade","Lucas","Manon","Yanis","Zoe","Adam")
    demo <- data.frame(id = as.integer(ids), pseudo = paste0(noms[seq_along(txt)], "_demo"),
                       equipe = sample(c("Les Viraux", "Team Hype", "Punchline Squad"), length(txt), TRUE),
                       texte = txt, auteur = paste0("demo_", ids), heure = Sys.time(),
                       score_ia = score_ia(f), f, stringsAsFactors = FALSE)
    # 25 votants fictifs qui votent un peu selon le "potentiel" des phrases
    poids <- exp(2.5 * demo$score_ia)
    t0 <- Sys.time() - 180
    vv <- do.call(rbind, lapply(1:25, function(k) {
      n <- min(BUZZ$jetons, length(ids))
      data.frame(voter_id = paste0("demo_votant_", k),
                 phrase_id = as.integer(sample(ids, n, prob = poids)),
                 heure = t0 + sort(runif(n, 0, 180)), stringsAsFactors = FALSE)
    }))
    BUZZ$phrases <- bind_rows(ph, demo)
    BUZZ$votes   <- bind_rows(BUZZ$votes, vv) |> arrange(heure)
    sauvegarder(); showNotification("Démo injectée 🧪", type = "message")
  })

  output$adm_export <- downloadHandler(
    filename = function() "buzzbattle_classe.csv",
    content = function(file) {
      d <- live_df() |> select(-auteur, -source) |> mutate(heure = format(heure, "%H:%M:%S"))
      write.csv2(d, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  observeEvent(input$adm_reset, {
    showModal(modalDialog(title = "Tout effacer ?",
                          "Phrases, votes et réglages seront remis à zéro. Action irréversible.",
                          footer = tagList(modalButton("Annuler"),
                                           actionButton("adm_reset_ok", "Oui, tout effacer", class = "btn-danger"))))
  })
  observeEvent(input$adm_reset_ok, {
    e <- etat_vide(); for (k in names(e)) BUZZ[[k]] <- e[[k]]
    sauvegarder(); removeModal()
    for (k in c("adm_depot", "adm_vote", "adm_reveal"))
      updateCheckboxInput(session, k, value = e[[c(adm_depot = "depot_ouvert", adm_vote = "vote_ouvert", adm_reveal = "reveal")[[k]]]])
    showNotification("Partie réinitialisée", type = "message")
  })
}

shinyApp(ui, server)
