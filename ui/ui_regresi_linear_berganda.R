# ui/ui_regresi_linear_berganda.R

ui_regresi_linear_berganda_tab <- fluidPage(
  h2("Regresi Linear Berganda"),
  p("Regresi linear berganda digunakan untuk memodelkan hubungan antara satu variabel dependen numerik dengan dua atau lebih variabel independen numerik."),
  
  # Bagian Atas: Konfigurasi Model dan Output Ringkasan (Kolom Berdampingan)
  fluidRow(
    column(6, # Kolom untuk Konfigurasi Model Regresi
           wellPanel(
             h3("Konfigurasi Model Regresi"),
             selectInput("reg_dependent_var", "Pilih Variabel Dependen (Numerik):",
                         choices = numeric_cols), # numeric_cols dari app.R
             
             # Ini akan dibuat secara dinamis di server
             uiOutput("reg_independent_vars_ui"), # UI Output untuk variabel independen
             
             actionButton("run_regression", "Jalankan Regresi", class = "btn btn-warning")
           )
    ),
    column(6, # Kolom untuk Output Model Regresi
           wellPanel(
             h3("Output Model Regresi"),
             tags$ul(
               tags$li(strong("Tujuan:"), "Memprediksi variabel dependen berdasarkan variabel independen."),
               tags$li(strong("Uji yang Digunakan:"), "F-test (untuk signifikansi keseluruhan model), t-test (untuk signifikansi koefisien individu)."),
               tags$li(strong("Interpretasi Koefisien:"), "Perubahan rata-rata variabel dependen untuk setiap peningkatan satu unit variabel independen, dengan variabel lain konstan. Untuk variabel kategorik, ini adalah perbedaan rata-rata dibandingkan dengan kategori referensi."),
               tags$li(strong("R-squared:"), "Proporsi varians variabel dependen yang dijelaskan oleh model.")
             ),
             verbatimTextOutput("regression_summary")
           )
    )
  ),
  
  hr(), # Garis pemisah visual
  
  # Bagian Tengah: Interpretasi Model Regresi (Full Width)
  fluidRow(
    column(12,
           wellPanel(
             h3("Interpretasi Model Regresi:"),
             verbatimTextOutput("regression_interpret")
           )
    )
  ),
  
  hr(), # Garis pemisah visual
  
  # Bagian Bawah: Uji Asumsi Regresi Linear (Full Width)
  fluidRow(
    column(12,
           wellPanel(
             h3("Uji Asumsi Regresi Linear"),
             p("Uji asumsi ini penting untuk memastikan validitas hasil regresi linear. Pelanggaran asumsi dapat menyebabkan kesimpulan yang bias atau tidak akurat."),
             
             # Asumsi 1: Linearitas (Plot Residuals vs Fitted)
             h4("1. Asumsi Linearitas"),
             p("Uji yang Digunakan: Inspeksi Visual Plot Residuals vs Fitted. Asumsi linearitas mengasumsikan hubungan linear antara variabel dependen dan independen."),
             p("Interpretasi: Jika asumsi terpenuhi, titik-titik (residuals) harus tersebar secara acak di sekitar garis horizontal y=0 tanpa pola yang jelas (misalnya, bentuk U, kerucut, atau kurva). Garis biru (lowess) harus mendekati garis merah (y=0). Pola yang jelas mengindikasikan pelanggaran asumsi linearitas."),
             plotOutput("residuals_fitted_plot"),
             verbatimTextOutput("linearity_interpret"),
             hr(),
             
             # Asumsi 2: Normalitas Residual
             h4("2. Asumsi Normalitas Residual"),
             p("Uji yang Digunakan: Anderson-Darling Test (ad.test dari paket `nortest`). Asumsi normalitas residual mengasumsikan bahwa error (residual) model terdistribusi normal."),
             tags$ul(
               tags$li(strong("Hipotesis Nol (H0):"), "Residual model berdistribusi normal."),
               tags$li(strong("Hipotesis Alternatif (H1):"), "Residual model tidak berdistribusi normal.")
             ),
             p(strong("Aturan Keputusan:"), "Tolak H0 jika nilai p < tingkat signifikansi (misalnya 0.05)."),
             verbatimTextOutput("normality_residuals_output"),
             verbatimTextOutput("normality_residuals_interpret"),
             hr(),
             
             # Asumsi 3: Homoskedastisitas (Konstansi Varians Residual)
             h4("3. Asumsi Homoskedastisitas"),
             p("Uji yang Digunakan: Non-Constant Variance Test (ncvTest dari paket `car`). Asumsi homoskedastisitas mengasumsikan bahwa varians residual adalah konstan di semua level variabel independen."),
             tags$ul(
               tags$li(strong("Hipotesis Nol (H0):"), "Varians residual adalah konstan (Homoskedastisitas)."),
               tags$li(strong("Hipotesis Alternatif (H1):"), "Varians residual tidak konstan (Heteroskedastisitas).")
             ),
             p(strong("Aturan Keputusan:"), "Tolak H0 jika nilai p < tingkat signifikansi (misalnya 0.05)."),
             verbatimTextOutput("homoscedasticity_output"),
             verbatimTextOutput("homoscedasticity_interpret"),
             hr(),
             
             # Asumsi 4: Independensi Residual
             h4("4. Asumsi Independensi Residual (Tidak Ada Autokorelasi)"),
             p("Uji yang Digunakan: Durbin-Watson Test (durbinWatsonTest dari paket `car`). Asumsi independensi residual mengasumsikan bahwa residual satu observasi tidak berkorelasi dengan residual observasi lain."),
             tags$ul(
               tags$li(strong("Hipotesis Nol (H0):"), "Residual bersifat independen (tidak ada autokorelasi)."),
               tags$li(strong("Hipotesis Alternatif (H1):"), "Residual tidak independen (ada autokorelasi).")
             ),
             p(strong("Aturan Keputusan:"), "Tolak H0 jika p-value < tingkat signifikansi (misalnya 0.05)."),
             verbatimTextOutput("durbin_watson_output"),
             verbatimTextOutput("durbin_watson_interpret"),
             hr(),
             
             # Asumsi 5: Tidak Ada Multikolinearitas
             h4("5. Asumsi Tidak Ada Multikolinearitas"),
             p("Uji yang Digunakan: Variance Inflation Factor (VIF dari paket `car`). Asumsi multikolinearitas mengasumsikan bahwa tidak ada korelasi tinggi antar variabel independen."),
             p(strong("Aturan Keputusan:"), "Nilai VIF > 5 (atau kadang > 10) menunjukkan adanya multikolinearitas yang signifikan."),
             verbatimTextOutput("vif_output"),
             verbatimTextOutput("vif_interpret")
           )
    )
  ),
  
  hr(), # Garis pemisah visual sebelum download laporan
  fluidRow(
    column(12,
           wellPanel(
             h3("Download Laporan Regresi Linear Berganda"),
             p("Unduh laporan lengkap yang mencakup konfigurasi model, output, interpretasi, dan hasil uji asumsi regresi linear dalam format RMarkdown (.Rmd). User harus mengkategorikan data
                terlebih dahulu untuk menggunakan fitur unduh ini dengan baik. User juga perlu mengunduh dataset awal dan data yang sudah dikategorikan yang berada 
                di menu Manajemen Data. Letakkan file Rmd Saudara di dalam satu file yang sama dengan file dataset dan data kategori untuk memudahkan dalam menjalankan 
                file Rmd yang sudah Saudara unduh. Harap tidak mengubah nama file dari dataset dan data kategori demi kenyamanan user sendiri. Terima kasih."),
             # --- Tambahkan tombol download laporan RMD Regresi Linear ---
             downloadButton("download_regresi_report", "Download Laporan Regresi (RMD)", class = "btn btn-warning")
           )
    )
  )
)