# ui/ui_uji_asumsi_data.R

ui_uji_asumsi_data_tab <- fluidPage(
  h2("Uji Asumsi Data"),
  p("Bagian ini memungkinkan Anda untuk melakukan uji normalitas dan homogenitas varians, yang merupakan asumsi penting dalam banyak analisis statistik parametrik."),
  
  fluidRow(
    # Kolom Kiri: Uji Normalitas
    column(6,
           wellPanel(
             h3("Uji Normalitas"),
             p("Uji normalitas digunakan untuk menentukan apakah sampel data berasal dari populasi yang berdistribusi normal."),
             tags$ul(
               tags$li(strong("Hipotesis Nol (H0):"), "Data berdistribusi normal."),
               tags$li(strong("Hipotesis Alternatif (H1):"), "Data tidak berdistribusi normal.")
             ),
             p(strong("Uji yang Digunakan:"), "Anderson-Darling Test (ad.test dari paket `nortest`)"),
             p(strong("Aturan Keputusan:"), "Tolak H0 jika nilai p < tingkat signifikansi (misalnya 0.05)."),
             selectInput("normality_var", "Pilih Variabel Numerik:",
                         choices = numeric_cols), # numeric_cols dari app.R
             actionButton("run_normality_test", "Jalankan Uji Normalitas", class = "btn btn-warning"),
             hr(),
             verbatimTextOutput("normality_test_output"),
             h4("Interpretasi Hasil Uji Normalitas:"),
             p("Interpretasi nilai p-value akan menunjukkan apakah ada bukti statistik yang cukup untuk menyimpulkan bahwa data tidak berdistribusi normal.")
             # --- BARIS INI DIHAPUS ---
             # downloadButton("download_normality_report", "Download Hasil Uji Normalitas (TXT)", class = "btn btn-warning")
             # --- AKHIR PENGHAPUSAN ---
           )
    ),
    
    # Kolom Kanan: Uji Homogenitas Varians
    column(6,
           wellPanel(
             h3("Uji Homogenitas Varians"),
             p("Uji homogenitas varians digunakan untuk menentukan apakah varians antar kelompok yang berbeda adalah sama. Ini sering menjadi asumsi untuk uji seperti ANOVA."),
             tags$ul(
               tags$li(strong("Hipotesis Nol (H0):"), "Varians antar kelompok adalah homogen (sama)."),
               tags$li(strong("Hipotesis Alternatif (H1):"), "Varians antar kelompok tidak homogen (berbeda).")
             ),
             p(strong("Uji yang Digunakan:"), "Levene's Test (leveneTest dari paket `car`)"),
             p(strong("Aturan Keputusan:"), "Tolak H0 jika nilai p < tingkat signifikansi (misalnya 0.05)."),
             selectInput("homogeneity_num_var", "Pilih Variabel Numerik:",
                         choices = numeric_cols),
             uiOutput("homogeneity_cat_var_ui"),
             actionButton("run_homogeneity_test", "Jalankan Uji Homogenitas", class = "btn btn-warning"),
             hr(),
             verbatimTextOutput("homogeneity_test_output"),
             h4("Interpretasi Hasil Uji Homogenitas:"),
             p("Interpretasi nilai p-value akan menunjukkan apakah varians variabel numerik secara signifikan berbeda antar kelompok yang ditentukan oleh variabel kategorik.")
             # --- BARIS INI DIHAPUS ---
             # downloadButton("download_homogeneity_report", "Download Hasil Uji Homogenitas (TXT)", class = "btn btn-warning")
             # --- AKHIR PENGHAPUSAN ---
           )
    )
  ),
  
  hr(), # Garis pemisah visual sebelum download laporan
  fluidRow(
    column(12,
           wellPanel(
             h3("Download Laporan Uji Asumsi Data"),
             p("Unduh laporan lengkap yang mencakup hasil uji normalitas dan homogenitas serta interpretasinya dalam format RMarkdown (.Rmd). User harus mengkategorikan data
                terlebih dahulu untuk menggunakan fitur unduh ini dengan baik. User juga perlu mengunduh dataset awal dan data yang sudah dikategorikan yang berada 
                di menu Manajemen Data. Letakkan file Rmd Saudara di dalam satu file yang sama dengan file dataset dan data kategori untuk memudahkan dalam menjalankan 
                file Rmd yang sudah Saudara unduh. Harap tidak mengubah nama file dari dataset dan data kategori demi kenyamanan user sendiri. Terima kasih."),
             downloadButton("download_asumsi_report", "Download Laporan Uji Asumsi (RMD)", class = "btn btn-warning")
           )
    )
  )
)