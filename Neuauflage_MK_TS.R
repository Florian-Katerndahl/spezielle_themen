coerce_to_numeric <- function(x) {
	# Macht aus Kontingenztabellen Vektoren. Ist das nötig? Keine Ahnung, schaden tut's aber nicht.
	
	if (is.numeric(x)) {
		return(x)
	}
	
	# Das sieht ein wenig komisch aus. Insbesondere da die Formatierung eines 1D tables Anderes vermuten lässt:
	# Ein Table hat keine Spalten.
	# if (!is.na(ncol(x)) | ncol(x) > 1) {
	# 	simpleError("Can't coerce x to a numeric vector. Please check, that the input is a 1D-Array/table with only rows.")
	# }
	
	return_message <-
			"Something went wrong while coercing to numeric. Please check your input!"
	
	return(
		tryCatch(as.numeric(x),
		         warning = function(e) stop("On Warning:\n", return_message),
		         error = function(e) stop("On Error:\n", return_message))
	)
}

MKScore <- function(x, y = NULL) {
	# x entspricht hier den y-Werten einer Zeitreihe.
	# Da beißen sich die Namenskonventionen leider ein wenig.
	
	if (is.null(y)) {
		# geht von durchgängiger Datenreihe aus, wenn nicht anders spezifiziert
		y <- seq(from = 1, to = length(x))
	}
	
	# n über 2 Anordnungen mit xi =/= xj
	comb_x <- combn(x, 2)
	
	comb_y <- combn(y, 2)
	
	result <- sum(
		pmap_dbl(
			# Reihenfolge weil j > i für 1 <= i < j <= n
			list(comb_x[2, ], comb_x[1, ], comb_y[2, ], comb_y[1, ]),
			function(xj, xi, yj, yi) sign(xj - xi) * sign(yj - yi)
		)
	)
	
	return(result)
}

MKVar <- function(n, t = NULL, u = NULL) {
	# Das entsprichter der varianz, in welcher in beiden Rangfolgen Wiederholungen/Gleichstände vorkommen dürfen (!).
	# Sollte das nicht der Fall sein und ein Rang nur einmal vorkommt, wird die entsprechende Summe gleich Null, weshalb nur
	# diese Form implementiert werden muss
	
	# Wenn t und/oder u nicht gegeben sind, wird davon ausgegangen, dass keine Doppelungen in entsprechender Rangfolge vorkommen.
	
	if (is.null(t)) {
		t <- rep(1, length.out = n)
	} else if (!is.table(t) | !is.array(t)) {
	  t <- table(t)
	}
	
	if (is.null(u)) {
		u <- rep(1, length.out = n)
	} else if (!is.table(u) | !is.array(u)) {
	  u <- table(u)
	}
	
	t <- coerce_to_numeric(t)
	
	u <- coerce_to_numeric(u)

	return(
		(((n * (n - 1) * (2 * n + 5)) - sum(t * (t - 1) * (2 * t + 5)) - sum(u * (u - 1) * (2 * u + 5))) +
			((1 / (9 * n * (n - 1) * (n - 2))) * sum(t * (t - 1) * (t - 2)) * sum(u * (u - 1) * (u - 2))) +
			((1 / (2 * n * (n - 1))) * sum(t * (t - 1)) * sum(u * (u - 1)))) / 18
	)
}

MKZ <- function(score, variance) {
	# Standardisierung der Teststatistik S
	
	if (score > 0) {
		return((score - 1) / sqrt(variance))
	} else if (score < 0) {
		return((score + 1) / sqrt(variance))
	} else {
		return(0)
	}
}

MKTau <- function(score, n, t = NULL, u = NULL) {
	# Berechnung von Tau_a, bzw. Tau_b. -> Extrading, was ich nicht direkt benötige.
	# Tau_a wird ausgespuckt, wenn t und u nicht angegeben werden.
	# Um mögliche Rundungsfehler zu vermeiden, wird nicht für beide Fälle die selbe Formel verwendet, was mathematisch möglich ist.
	# Wenn nur t oder u angegeben werden wird davon ausgegangen, dass in der jeweils fehlenden Rangfolge keine Wiederholungen auftreten
	# und es wird Tau_b berechnet.
	
  # score   ->    Kendalls S
  # n       ->    Anzahl an Beobachtungen
  # t       ->    Kontingenztabelle der Ränge in t. Wenn nicht angegeben wird ein Vektor der Länge N mit natürlichen Zahlen erstellt.
  #               Das entpspricht einer Kontigenztabelle, wenn alle Werte einzigartig sind.
  # u       ->    siehe t...
  
	if (is.null(t) & is.null(u)) {
		result <- score / choose(n, 2)
		
		names(result) <- "Tau a"
		
		return(result)
	} 
  if (is.null(t)) {
		t <- rep(1, length.out = n)
	} else if (!is.table(t) | !is.array(t)) {
	  t <- table(t)
	}
  
  if (is.null(u)) {
		u <- rep(1, length.out = n)
  } else if (!is.table(u) | !is.array(u)) {
    u <- table(u)
  }
	
	t <- coerce_to_numeric(t)
	
	u <- coerce_to_numeric(u)
	
	result <- score / (sqrt(choose(n, 2) - (0.5 * sum(t * (t - 1)))) * sqrt(choose(n, 2) - (0.5 * sum(u * (u - 1)))))
	
	names(result) <- "Tau b"
	
	return(round(result, 2))
}

MKTest <- function(Y, t = NULL, alpha_val = 0.05, alternative = c("two.sided", "less", "greater")) {
  # Testet die Rangkorrelation auf Signifikanz. Wenn alternative = "two.sided", dann alpha/2.
  
  # Y       ->    y-Werte der Zeitreihe
  # t       ->    x-Werte der Zeitreihe
  # alpha   ->    Signifikanzniveau.
  # alternative ->  Was ist die H1-Hypothese
  
  if (length(Y) != length(t) & !is.null(t)) {
    stop("Vectors are not of same length while t was supplied.")
  } else if (is.null(t)) {
    t <- seq(from = 1, to = length(Y))
  }
  
  n <- length(t)
  
  score <- MKScore(x = Y, y = t)
  
  variance <- MKVar(n = n, t = t, u = Y)
  
  Z <- MKZ(score = score, variance = variance)
  
  Tau <- MKTau(score = score, n = n, t = t, u = Y)
  
  alternative <- match.arg(alternative)
  
  # Credits siehe TSTest
  pval <- switch(
    alternative,
    two.sided = 2 * min(0.5, pnorm(abs(Z), lower.tail = FALSE)),
    less = pnorm(Z, lower.tail = TRUE),
    greater = pnorm(Z, lower.tail = FALSE)
  )
  
  return(
    list(
      "S" = score,
      "Z" = Z,
      "Tau" = Tau,
      "alpha" = alpha_val,
      "alternative" = alternative,
      "p-Wert" = pval,
      "trend_significant" = ifelse(pval < alpha_val, TRUE, FALSE)
    )
  )
}

TSSlope <- function(Y, t = NULL) {
	# t entspricht hier den x Werten, wobei Doppelungen/wiederholungen erlaubt sind.
	# es dürfen nicht alle t identisch sein.
	# Ebenso kann keine Steigung berechnet werden, wenn tj - ti = 0. 
  
  # Y       ->    y-Werte der Zeitreihe
  # t       ->    x-Werte der Zeitreihe
	
	if (is.null(t)) {
		t <- seq(from = 1, to = length(Y))
	}
	
	t <- coerce_to_numeric(t)
	
	comb_t <- combn(t, 2)
	
	comb_Y <- combn(Y, 2)
	
	result <- median(
		pmap_dbl(
			list(comb_Y[2, ], comb_Y[1, ], comb_t[2, ], comb_t[1, ]),
			function(Yj, Yi, tj, ti) {
				if (!(tj > ti) | ((tj - ti) == 0)) {
					return(NA)
				}
				
				(Yj - Yi) / (tj - ti)
			}
		)
	, na.rm = TRUE)
	
	return(result)
}

TSScore <- function(Y, slope, t = NULL) {
	# Berechnet das Theil-Sen Äquivalent zur Mann-Kendall Teststatistik S.
	# Ich habe mich entschlossen, mich am Paper zu orientieren und die bereinigten y Werte (welche verwirrender weise mit Z notiert werden)
	# zu verwenden. Im Gegensatz zu dem Paket "trend"
	# t hier nicht anzugeben ergibt natürlich nur dann Sinn, wenn auch bei der Berechnung der Steigung t nicht angegeben wurde!
	
	if (is.null(t)) {
		t <- seq(from = 1, to = length(Y))
	}
	
	t <- coerce_to_numeric(t)
	
	coeff_T_t <- slope * t
	
	Z <- Y - coeff_T_t
	
	comb_t <- combn(t, 2)
	
	comb_Z <- combn(Z, 2)
	
	result <- sum(
		pmap_dbl(
			list(comb_t[2, ], comb_t[1, ], comb_Z[2, ], comb_Z[1, ]),
			function(tj, ti, Zj, Zi) sign(tj - ti) * sign(Zj - Zi)
		)
	)
	
	return(list(
	  "coeff" = coeff_T_t,
	  "result" = result
	  ))
}

TSVar <- function(n, u = NULL) {
  # Berechnet die varianz der Teststatistik der Theil-Sen Schätzung
  # n       ->    Anzahl an Beobachtungen
  # u       ->    Doppelungen/Wiederholungen in Rangfolge t. Entgegen der Namensgebung sind das die Doppelungen in den "x-Werten".

  if (is.null(u)) {
    u <- rep(1, length.out = n)
  } else if (!is.table(u) | !is.array(u)) {
    u <- table(u)
  }
  
  u <- coerce_to_numeric(u)
  
  return(
    ((n * (n - 1) * (2 * n + 5)) - sum(u * (u - 1) * (2 * u + 5))) / 18
  )
}

TSZ <- function(score, variance) {
  # Berechnung der standardisierten Teststatistik. Ist identisch mit der standardisierten Teststatistik Z vom Mann-Kendall Test
  
  # score   ->    Theil-Sen Äquivalent zu Kendalls S
  # variance  ->  Varianz der Teststatistik
  return(MKZ(score, variance))
}

.TSN <- function(t) {
  # Berechnet N nach Sen 2.1
  # genaueres in Beschreibung von TSU
  
  comb_t <- combn(t, 2)
  
  return(
    sum(
      map2_dbl(comb_t[2, ], comb_t[1, ], 
               function(t2, t1) sign(t2 - t1)
               )
      )
  )
}

TSU <- function(score, n, t = NULL) {
	# NOCH NICHT FERTIG, WARTE AUF RÜCKMELDUNG VON MARION -> Tau b Äquivalent mit Doppelungen in beiden Rangfolgen
	# Berechnet das Theil-Sen Äquivalent zu Kendall's Tau. Eine Differenzierung in Tau a und Tau b gibt es bei Sen nicht, wenn
  # ich das Paper richtig verstehe.
  # Unter anderem auf Seite 1380 direkt unter Gleichung 2.2 und auf Seite 1381 unter Gleichung 2.6 wird deutlich, dass das U Kendalls Tau
  # entspricht.
	
	# Der Zähler hier besteht aus der Wurzel des Produkts aus N ["number of positive differences tj - ti, so that N <= choose(n, 2)"]
  # und choose(n, 2).
  # N entspricht ein Kendalls S, berechnet auf nur einer Rangfolge. 
  # Der gesamte Term [(N * choose(n, 2)) ^ (-0.5)] findet zu einem gewissen Grad seine Entsprechung  auf Seite 35 in Kendall (1975):
  # ... / (sqrt(choose(n, 2) - T mit T = 0.5 * sum(t * (t - 1))) * ...)
  # Während die Formel im Kontext von 2.2 leichter zu verstehen ist, ist es einfacher N separat zu berechnen.
  
  if (is.null(t)) {
    t <- seq(1, length.out = n)
  }
  
  N <- .TSN(t)
	
  return(
      (1 / sqrt(N * choose(n, 2))) * score
  )
}

TSTest <- function(Y, t = NULL, alpha_val = 0.05) {
  # Test auf Signifikanz der Theil-Sen Steigung. Diese ist dann signifikant, wenn keine signifikante Rangkorrelation
  # existiert (siehe Theil).
  
  # Y       ->    y-Werte der Zeitreihe
  # t       ->    x-Werte der Zeitreihe
  # alpha   ->    Signifikanzniveau. Hier wird immer in beide Richtungen getestet, die "alternative" ist also immer "two.sided".
  
  if (length(Y) != length(t) & !is.null(t)) {
    stop("Vectors are not of same length while t was supplied.")
  } else if (is.null(t)) {
    t <- seq(from = 1, to = length(Y))
  }
  
  n <- length(t)
  
  slope <- TSSlope(Y = Y, t = t)
  
  dummy <- TSScore(Y = Y, t = t, slope = slope)
  
  score <- dummy[["result"]]
  
  variance <- TSVar(n = n, u = t)
  
  Z <- TSZ(score = score, variance = variance)
  
  U <- TSU(score = score, n = n, t = t)
  
  # 2 x die Überschreitungswahrscheinlichkeit von |Z| entspricht pnorm(-Z, lower.tail = TRUE) + pnorm(Z, lower.tail = FALSE)
  # min(0.5, ...) garantiert einen Wert, der nicht > 1 sein kann.
  # Die Zeile vom Paket "trend" (Thorsten Pohlert (2020)) geklaut.
  pval <- 2 * min(0.5, pnorm(abs(Z), lower.tail = FALSE))
  
  return(
    list(
      "coeff" = dummy[[1]],
      "Theil-Sen Steigung" = slope,
      "Theil-Sen Teststatistik" = Z,
      "Theil-Sens 'Tau'" = U,
      "Konfidenzinterval" = 1 - alpha_val,
      "p-Wert" = pval,
      "slope_signifcant" = ifelse(pval > alpha_val, TRUE, FALSE)
    )
  )
}

calculate_autocorrelation <- function(Y) {
  return(acf(Y, plot = FALSE, na.action = na.pass)$acf[2])
}

adjust_ts <- function(Y, coeff) {
  Y_desloped <- Y - coeff
  
  ar <- calculate_autocorrelation(Y_desloped)
  
  Y_hat <- Y_desloped - ar * lag(Y_desloped)
  
  Y_ready <- Y_hat + coeff
  
  return(
    # Der erste Wert wird hier NA durch lag(Y_desloped)
    Y_ready[2:length(Y_ready)]
  )
}
