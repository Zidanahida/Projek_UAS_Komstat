# ui/ui_beranda.R

ui_beranda_tab <- fluidPage(
  # Header Utama
  h2("Selamat Datang di (DAD'S) Dashboard Analisis Data SoVI"),
  
  # Ringkasan Statistik Data
  h3("Ringkasan Statistik Data"),
  fluidRow(
    column(3, wellPanel( # Ganti bslib::bs_card dengan wellPanel
      div(style = "text-align: center; font-size: 2em; font-weight: bold;", "511"),
      div(style = "text-align: center; font-size: 1.2em;", "Kabupaten Kota")
    )),
    column(3, wellPanel( # Ganti bslib::bs_card dengan wellPanel
      div(style = "text-align: center; font-size: 2em; font-weight: bold;", "17"),
      div(style = "text-align: center; font-size: 1.2em;", "Variabel Data")
    )),
    column(3, wellPanel( # Ganti bslib::bs_card dengan wellPanel
      div(style = "text-align: center; font-size: 2em; font-weight: bold;", "2017"),
      div(style = "text-align: center; font-size: 1.2em;", "Tahun Data")
    )),
    column(3, wellPanel( # Ganti bslib::bs_card dengan wellPanel
      div(style = "text-align: center; font-size: 2em; font-weight: bold;", "BPS"),
      div(style = "text-align: center; font-size: 1.2em;", "Sumber Data")
    ))
  ),
  hr(), # Garis pemisah
  
  # Tentang Dashboard
  h3("Tentang Dashboard"),
  p("Dashboard interaktif ini dirancang sebagai alat untuk melakukan eksplorasi dan analisis statistik terhadap data Kerentanan Sosial di 511 kabupaten/kota di Indonesia. Proyek ini disusun untuk memenuhi tugas Ujian Akhir Semester (UAS) mata kuliah Komputasi Statistik. Tujuan utama dashboard ini adalah menyediakan antarmuka yang ramah pengguna untuk menganalisis data multi-dimensi, mulai dari statistik deskriptif hingga analisis inferensial yang lebih kompleks."),
  
  # Fitur Utama Dashboard
  h3("Fitur Utama Dashboard"),
  tags$ul(
    tags$li(strong("Manajemen Data:"), "Menyediakan fasilitas untuk mengelola data, termasuk kemampuan untuk mengubah variabel kontinyu menjadi variabel kategorik untuk analisis lebih lanjut"),
    tags$li(strong("Eksplorasi Data:"), "Menampilkan statistik deskriptif, tabel, serta beragam visualisasi data seperti grafik dan peta tematik untuk memahami karakteristik data secara mendalam"),
    tags$li(strong("Analisis Inferensial:"), "uji beda rata-rata, uji proporsi, ANOVA"),
    tags$li(strong("Analisis regresi berganda:"), "beserta informasi asumsinya"),
    tags$li(strong("Fungsi unduh data:"), "dalam format Excel dan unduh laporan dalam word")
  ),
  
  # Sumber Data Link
  h3("Sumber Data:"),
  p(a(href = "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv", "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv")),
  
  hr(), # Garis pemisah
  
  # Deskripsi Variabel Data SOVI (Tabel)
  h3("Deskripsi Variabel Data SOVI"),
  DTOutput("variable_description_table") # Output tabel yang akan dirender dari server
)