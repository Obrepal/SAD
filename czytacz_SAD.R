# Problem 2 – sygnał z detektora

# --------------------------------------------------------------------------
# WCZYTANIE SYGNAŁU
# --------------------------------------------------------------------------

filename <- 'signal_50MHz.bin'
zz <- file(filename, "rb")

BajtowNaLiczbe = 4
fsize = file.size(filename)
LiczbaLiczb = fsize / BajtowNaLiczbe
v <- readBin(zz, numeric(), size = BajtowNaLiczbe, endian = "little", n = LiczbaLiczb)
close(zz)

# --------------------------------------------------------------------------
# PODPUNKT A - Badanie rozkładu szumu
# --------------------------------------------------------------------------

png("sygnal_bez_impulsow.png")
plot(v[1000:2000], type = 'l', col = "blue", main = "Fragment sygnału bez impulsów", xlab = "Próbki", ylab = "Amplituda")
dev.off()

png("sygnal_z_impulsami.png")
plot(v[1:100000], type = 'l', col = "red", main = "Fragment sygnału z impulsami", xlab = "Próbki", ylab = "Amplituda")

idx = 67650:67900;
png("impuls_zblizenie.png")
plot(idx, v[idx], type = 'l', col = "blue", main = "Fragment sygnału z impulsem", xlab = "Próbki", ylab = "Amplituda")    
dev.off()

idx = 1:17000000; # Po próbce 17000000 nie ma żadnego sygnału
png("caly_sygnal.png")
plot(idx, v[idx], type = 'l', col = "blue", main = "Cały sygnał", xlab = "Próbki", ylab = "Amplituda")      
dev.off()

noise_vec = v[1:60000]
mean = mean(noise_vec)
std_dev = sd(noise_vec)
max = max(noise_vec)
min = min(noise_vec)

cat("Number of samples:", length(noise_vec))
cat("\nMean:", mean, "\nStandard deviation:", std_dev, "\n")
cat("Max:", max, "\nMin:", min, "\n")

png("histogram_szumu.png")
hist(noise_vec, breaks = 30, main = "Histogram i rozkład normalny zarejestrowanego szumu", xlab = "Amplituda szumu", ylab = "Częstość", col = "#cf7527", freq = FALSE)
dev.off()

png("histogram_szumu_i_rozklad_normalny.png")
hist(noise_vec, breaks = 30, main = "Histogram i rozkład normalny zarejestrowanego szumu", xlab = "Amplituda szumu", ylab = "Częstość", col = "#cf7527", freq = FALSE)
curve(dnorm(x, mean = mean, sd = std_dev),
    add = TRUE,
    from = mean - 4 * std_dev,
    to = mean + 4 * std_dev,
    col = "purple",
    lwd = 3)
abline(v = mean, col = "blue", lty = 2)  # mean
abline(v = mean + std_dev, col = "red", lty = 2)  # +1 sigma
abline(v = mean - std_dev, col = "red", lty = 2)  # -1 sigma
abline(v = mean + 2 * std_dev, col = "green", lty = 2)  # +2 sigma
abline(v = mean - 2 * std_dev, col = "green", lty = 2)  # -2 sigma
abline(v = mean + 3 * std_dev, col = "blue", lty = 2)  # +3 sigma
abline(v = mean - 3 * std_dev, col = "blue", lty = 2)  # -3 sigma
text(mean + std_dev, dnorm(mean + std_dev, mean, std_dev), labels = "+1σ", pos = 3, cex = 0.7)
text(mean - std_dev, dnorm(mean - std_dev, mean, std_dev), labels = "-1σ", pos = 3, cex = 0.7)
text(mean + 2 * std_dev, dnorm(mean + 2 * std_dev, mean, std_dev), labels = "+2σ", pos = 3, cex = 0.7)
text(mean - 2 * std_dev, dnorm(mean - 2 * std_dev, mean, std_dev), labels = "-2σ", pos = 3, cex = 0.7)
text(mean + 3 * std_dev, dnorm(mean + 3 * std_dev, mean, std_dev), labels = "+3σ", pos = 3, cex = 0.7)
text(mean - 3 * std_dev, dnorm(mean - 3 * std_dev, mean, std_dev), labels = "-3σ", pos = 3, cex = 0.7)
dev.off()

q <- seq(-1, 1, by = 0.1)
cdf <- pnorm(q, mean = mean, sd = std_dev)
png("dystrybuanta_szumu.png")
plot(q, cdf, type = "l", main = "Dystrybuanta zarejestrowanego szumu",
     xlab = "Kwantyl", ylab = "Prawdopodobieństwo", col = "#247343", lwd = 3)
dev.off()

png("wykres_pudelkowy.png")
boxplot(noise_vec, main = "Wykres pudełkowy", ylab = "Wartości szumu", col = "#e37e36", border = "#2d4db5")
dev.off()

# --------------------------------------------------------------------------
# PODPUNKT B - Badanie rozkładu odstępów czasu pomiędzy kolejnymi impulsami.
# --------------------------------------------------------------------------

noise_vec = v[1:60000]
mean = mean(noise_vec)
std_dev = sd(noise_vec)
max = max(noise_vec)
min = min(noise_vec)

cat("Number of samples:", length(noise_vec))
cat("\nMean:", mean, "\nStandard deviation:", std_dev, "\n")
cat("Max:", max, "\nMin:", min, "\n")

idx = 67000:76000;
png("impuls_zblizenie_2.png")
plot(idx, v[idx], type = 'l', col = "blue", main = "Fragment sygnału z dwoma impulsami", xlab = "Próbki", ylab = "Amplituda", lwd = 2)      # jeden impuls - aby zobaczyć kształt impulsów
dev.off()

Algorytm do wykrywania impulsów
signal = v
threshold_1 = 0.01
threshold_2 = 0.002
impulse_detected = FALSE
impulse_sample_diff_vec <- c()
current_sample_id = 1
previous_impulse_sample_id = 1

for (sample_value in signal) {
    if (impulse_detected == FALSE) {
        if (sample_value > threshold_1) {
            impulse_detected = TRUE
            # Jeśli wykryjemy pierwszy impuls to nie zapisujemy czasu od początku trwania sygnału do wystąpienia pierwszego impulsu
            if (previous_impulse_sample_id != 1) {
                impulse_sample_diff_vec <- c(impulse_sample_diff_vec, current_sample_id - previous_impulse_sample_id)
            }
            previous_impulse_sample_id = current_sample_id;
        }
    } else {
        if (sample_value < threshold_2) {
            impulse_detected = FALSE
        }
    }
    current_sample_id <- current_sample_id + 1
}

cat("Number of impulses detected:", length(impulse_sample_diff_vec) + 1, "\n")
print(impulse_sample_diff_vec)

# Histogram odstępów czasu pomiędzy kolejnymi impulsami
png("histogram_odstepow_miedzy_impulsami.png")
hist(impulse_sample_diff_vec, breaks = 30, main = "Histogram odstępów pomiędzy kolejnymi impulsami", xlab = "Odstęp w próbkach", ylab = "Częstość", col = "blue", freq = TRUE)
dev.off()
