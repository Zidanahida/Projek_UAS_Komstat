# ui/ui_statistik_inferensia.R

ui_statistik_inferensia_tab <- fluidPage(
  h2("Statistik Inferensia"),
  tabsetPanel(
    id = "inferensia_sub_menu",
    
    # Sub-menu 1: Uji Beda Rata-rata (t-test)
    tabPanel("Uji Beda Rata-rata",
             sidebarLayout(
               sidebarPanel(
                 h3("Uji Beda Rata-rata"),
                 p("Uji ini membandingkan rata-rata satu atau dua kelompok."),
                 
                 # Bagian 1: Uji t 1 Kelompok (One-Sample t-test)
                 h4("1. Uji t 1 Kelompok (One-Sample t-test)"),
                 selectInput("one_sample_t_var", "Pilih Variabel Numerik:", choices = numeric_cols),
                 numericInput("one_sample_t_mu", "Nilai Hipotesis Rata-rata (μ0):", value = 0, step = 0.1),
                 selectInput("one_sample_t_alt", "Hipotesis Alternatif:",
                             choices = c("Rata-rata tidak sama dengan μ0 (two.sided)" = "two.sided",
                                         "Rata-rata kurang dari μ0 (less)" = "less",
                                         "Rata-rata lebih dari μ0 (greater)" = "greater")),
                 actionButton("run_one_sample_t", "Jalankan Uji 1 Kelompok", class = "btn btn-warning"),
                 hr(),
                 
                 # Bagian 2: Uji t 2 Kelompok (Two-Sample t-test)
                 h4("2. Uji t 2 Kelompok (Two-Sample t-test)"),
                 selectInput("two_sample_t_num_var", "Pilih Variabel Numerik:", choices = numeric_cols),
                 # Variabel Kategorik akan diisi dinamis dari server
                 uiOutput("two_sample_t_cat_var_ui"),
                 selectInput("two_sample_t_type", "Jenis Uji t-test:",
                             choices = c("Independen (Varians diasumsikan sama)" = "independent_equal_var",
                                         "Independen (Varians diasumsikan berbeda - Welch)" = "independent_unequal_var",
                                         "Berpasangan" = "paired")),
                 selectInput("two_sample_t_alt", "Hipotesis Alternatif:",
                             choices = c("Rata-rata tidak sama (two.sided)" = "two.sided",
                                         "Rata-rata kelompok 1 kurang dari kelompok 2 (less)" = "less",
                                         "Rata-rata kelompok 1 lebih dari kelompok 2 (greater)" = "greater")),
                 actionButton("run_two_sample_t", "Jalankan Uji 2 Kelompok", class = "btn btn-warning")
               ),
               mainPanel(
                 h3("Hasil Uji Beda Rata-rata"),
                 
                 h4("Uji t 1 Kelompok (One-Sample t-test)"),
                 tags$ul(
                   tags$li(strong("Hipotesis Nol (H0):"), "Rata-rata populasi (μ) sama dengan nilai hipotesis (μ0)."),
                   tags$li(strong("Hipotesis Alternatif (H1):"), "Rata-rata populasi (μ) tidak sama dengan, kurang dari, atau lebih dari nilai hipotesis (μ0)."),
                   tags$li(strong("Uji yang Digunakan:"), "One-Sample t-test."),
                   tags$li(strong("Aturan Keputusan:"), "Tolak H0 jika p-value < tingkat signifikansi (misalnya 0.05).")
                 ),
                 verbatimTextOutput("one_sample_t_output"),
                 p(""),
                 h4("Interpretasi Uji 1 Kelompok:"),
                 verbatimTextOutput("one_sample_t_interpret"),
                 hr(),
                 
                 h4("Uji t 2 Kelompok (Two-Sample t-test)"),
                 tags$ul(
                   tags$li(strong("Hipotesis Nol (H0):"), "Rata-rata kedua populasi adalah sama (μ1 = μ2)."),
                   tags$li(strong("Hipotesis Alternatif (H1):"), "Rata-rata kedua populasi tidak sama, atau satu lebih besar/kecil dari yang lain."),
                   tags$li(strong("Uji yang Digunakan:"), "Two-Sample t-test (Independen atau Berpasangan, tergantung pilihan)."),
                   tags$li(strong("Aturan Keputusan:"), "Tolak H0 jika p-value < tingkat signifikansi (misalnya 0.05).")
                 ),
                 verbatimTextOutput("two_sample_t_output"),
                 p(""),
                 h4("Interpretasi Uji 2 Kelompok:"),
                 verbatimTextOutput("two_sample_t_interpret")
               )
             )
    ),
    
    # Sub-menu 2: Uji Proporsi & Varians
    tabPanel("Uji Proporsi & Varians",
             sidebarLayout(
               sidebarPanel(
                 h3("Uji Proporsi & Varians"),
                 
                 # Bagian 1: Uji Proporsi
                 h4("1. Uji Proporsi"),
                 selectInput("prop_test_type", "Pilih Jenis Uji Proporsi:", choices = c("1 Kelompok", "2 Kelompok")),
                 conditionalPanel(
                   condition = "input.prop_test_type == '1 Kelompok'",
                   numericInput("prop_1_x", "Jumlah Kejadian (x):", value = 10, min = 0),
                   numericInput("prop_1_n", "Jumlah Total Observasi (n):", value = 20, min = 1),
                   numericInput("prop_1_p", "Proporsi Hipotesis (p0):", value = 0.5, min = 0, max = 1, step = 0.01),
                   selectInput("prop_1_alt", "Hipotesis Alternatif:",
                               choices = c("Proporsi tidak sama dengan p0 (two.sided)" = "two.sided",
                                           "Proporsi kurang dari p0 (less)" = "less",
                                           "Proporsi lebih dari p0 (greater)" = "greater"))
                 ),
                 conditionalPanel(
                   condition = "input.prop_test_type == '2 Kelompok'",
                   numericInput("prop_2_x1", "Jumlah Kejadian Kelompok 1 (x1):", value = 10, min = 0),
                   numericInput("prop_2_n1", "Jumlah Total Kelompok 1 (n1):", value = 20, min = 1),
                   numericInput("prop_2_x2", "Jumlah Kejadian Kelompok 2 (x2):", value = 15, min = 0),
                   numericInput("prop_2_n2", "Jumlah Total Kelompok 2 (n2):", value = 30, min = 1)
                 ),
                 actionButton("run_prop_test", "Jalankan Uji Proporsi", class = "btn btn-warning"),
                 hr(),
                 
                 # Bagian 2: Uji Varians (untuk 2 Kelompok)
                 h4("2. Uji Varians (untuk 2 Kelompok)"),
                 p("Membandingkan varians dua kelompok. Uji varians 1 kelompok umumnya dilakukan dengan Chi-square test pada varians sampel, yang tidak termasuk fungsi dasar di sini."),
                 selectInput("var_test_num_var", "Pilih Variabel Numerik:", choices = numeric_cols),
                 uiOutput("var_test_cat_var_ui"),
                 selectInput("var_test_alt", "Hipotesis Alternatif:",
                             choices = c("Rasio varians tidak sama dengan 1 (two.sided)" = "two.sided",
                                         "Rasio varians kurang dari 1 (less)" = "less",
                                         "Rasio varians lebih dari 1 (greater)" = "greater")),
                 actionButton("run_var_test", "Jalankan Uji Varians", class = "btn btn-warning")
               ),
               mainPanel(
                 h3("Hasil Uji Proporsi & Varians"),
                 
                 h4("Uji Proporsi"),
                 tags$ul(
                   tags$li(strong("Hipotesis Nol (H0) - 1 Kelompok:"), "Proporsi populasi (p) sama dengan nilai hipotesis (p0)."),
                   tags$li(strong("Hipotesis Alternatif (H1) - 1 Kelompok:"), "Proporsi populasi (p) tidak sama dengan, kurang dari, atau lebih dari p0."),
                   tags$li(strong("Hipotesis Nol (H0) - 2 Kelompok:"), "Proporsi kedua populasi adalah sama (p1 = p2)."),
                   tags$li(strong("Hipotesis Alternatif (H1) - 2 Kelompok:"), "Proporsi kedua populasi tidak sama, atau satu lebih besar/kecil dari yang lain."),
                   tags$li(strong("Uji yang Digunakan:"), "Prop.test."),
                   tags$li(strong("Aturan Keputusan:"), "Tolak H0 jika p-value < tingkat signifikansi (misalnya 0.05).")
                 ),
                 verbatimTextOutput("prop_test_output"),
                 p(""),
                 h4("Interpretasi Uji Proporsi:"),
                 verbatimTextOutput("prop_test_interpret"),
                 hr(),
                 
                 h4("Uji Varians (untuk 2 Kelompok)"),
                 tags$ul(
                   tags$li(strong("Hipotesis Nol (H0):"), "Varians kedua populasi adalah sama (σ1² = σ2²)."),
                   tags$li(strong("Hipotesis Alternatif (H1):"), "Varians kedua populasi tidak sama, atau satu lebih besar/kecil dari yang lain."),
                   tags$li(strong("Uji yang Digunakan:"), "F-test (var.test)."),
                   tags$li(strong("Aturan Keputusan:"), "Tolak H0 jika p-value < tingkat signifikansi (misalnya 0.05).")
                 ),
                 verbatimTextOutput("var_test_output"),
                 p(""),
                 h4("Interpretasi Uji Varians:"),
                 verbatimTextOutput("var_test_interpret")
               )
             )
    ),
    
    # Sub-menu 3: ANOVA
    tabPanel("ANOVA",
             sidebarLayout(
               sidebarPanel(
                 h3("Analisis Varians (ANOVA)"),
                 p("Uji ini membandingkan rata-rata dari dua atau lebih kelompok."),
                 selectInput("anova_type", "Pilih Jenis ANOVA:", choices = c("One-Way ANOVA", "Two-Way ANOVA")),
                 
                 # One-Way ANOVA
                 conditionalPanel(
                   condition = "input.anova_type == 'One-Way ANOVA'",
                   selectInput("anova_1w_num_var", "Pilih Variabel Numerik (Dependent):", choices = numeric_cols),
                   uiOutput("anova_1w_cat_var_ui") # Variabel kategorik (Independent Factor)
                 ),
                 
                 # Two-Way ANOVA
                 conditionalPanel(
                   condition = "input.anova_type == 'Two-Way ANOVA'",
                   selectInput("anova_2w_num_var", "Pilih Variabel Numerik (Dependent):", choices = numeric_cols),
                   uiOutput("anova_2w_cat_var1_ui"), # Variabel kategorik Faktor 1
                   uiOutput("anova_2w_cat_var2_ui"), # Variabel kategorik Faktor 2
                   checkboxInput("anova_2w_interaction", "Sertakan Interaksi?", value = TRUE)
                 ),
                 actionButton("run_anova", "Jalankan ANOVA", class = "btn btn-warning")
               ),
               mainPanel(
                 h3("Hasil ANOVA"),
                 tags$ul(
                   tags$li(strong("Hipotesis Nol (H0):"), "Rata-rata semua kelompok adalah sama."),
                   tags$li(strong("Hipotesis Alternatif (H1):"), "Setidaknya ada satu kelompok yang rata-ratanya berbeda dari yang lain."),
                   tags$li(strong("Uji yang Digunakan:"), "Analisis Varians (ANOVA)."),
                   tags$li(strong("Aturan Keputusan:"), "Tolak H0 jika p-value < tingkat signifikansi (misalnya 0.05).")
                 ),
                 verbatimTextOutput("anova_output"),
                 p(""),
                 h4("Interpretasi Hasil ANOVA:"),
                 verbatimTextOutput("anova_interpret")
               )
             )
    ),
    
    # Sub-menu 4: Download Laporan Inferensia (BARU)
    tabPanel("Download Laporan",
             fluidRow(
               column(12,
                      wellPanel(
                        h3("Download Laporan Statistik Inferensia"),
                        p("Unduh laporan lengkap yang mencakup hasil uji statistik inferensia (Uji-t, Uji Proporsi, Uji Varians, ANOVA) serta interpretasinya dalam format RMarkdown (.Rmd). User harus mengkategorikan data
                           terlebih dahulu untuk menggunakan fitur unduh ini dengan baik. User juga perlu mengunduh dataset awal dan data yang sudah dikategorikan yang berada 
                           di menu Manajemen Data. Letakkan file Rmd Saudara di dalam satu file yang sama dengan file dataset dan data kategori untuk memudahkan dalam menjalankan 
                           file Rmd yang sudah Saudara unduh. Harap tidak mengubah nama file dari dataset dan data kategori demi kenyamanan user sendiri. Terima kasih."),
                        downloadButton("download_inferensia_report", "Download Laporan Statistik Inferensia (RMD)", class = "btn btn-warning")
                      )
               )
             )
    )
  )
)