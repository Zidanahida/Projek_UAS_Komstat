# ui/ui_manajemen_data.R

ui_manajemen_data_tab <- fluidPage(
  h2("Manajemen Data"),
  tabsetPanel(
    id = "manajemen_data_sub_menu",
    
    # Sub-menu 1: Tabel Data Kontinyu
    tabPanel("Tabel Data Kontinyu",
             fluidRow(
               column(12,
                      h3("Seluruh Data Kontinyu (sovi_data.csv)"),
                      p("Ini adalah tampilan lengkap dari dataset asli yang Anda gunakan."),
                      # --- BARIS BARU INI ---
                      downloadButton("download_sovi_csv", "Download Dataset Asli (CSV)", class = "btn btn-warning"),
                      hr(),
                      DTOutput("full_sovi_data_table")
               )
             )
    ),
    
    # Sub-menu 2: Transformasi Data (Fungsi Kategorisasi yang Sudah Ada)
    tabPanel("Transformasi Data",
             fluidPage( # Menggunakan fluidPage untuk menampung tata letak kustom
               fluidRow( # Baris pertama untuk kolom berdampingan
                 column(6, # Kolom kiri untuk kontrol input
                        wellPanel( # wellPanel untuk memberikan latar belakang abu-abu yang terkotak
                          h3("Kategorisasi Data Kontinu"),
                          selectInput("var_to_categorize", "Pilih Variabel Kontinu:",
                                      choices = numeric_cols),
                          numericInput("num_breaks", "Jumlah Kategori (Breaks):",
                                       value = 5, min = 2, max = 10),
                          selectInput("categorization_method", "Pilih Metode Kategorisasi:",
                                      choices = c("Natural Breaks (Jenks)" = "jenks",
                                                  "Equal Interval" = "equal",
                                                  "Quantile" = "quantile",
                                                  "Pretty Breaks" = "pretty",
                                                  "Hierarchical Cluster" = "hclust")),
                          # --- Tombol "Kategorikan Data" ---
                          actionButton("categorize_btn", "Kategorikan Data", class = "btn btn-warning"),
                          hr(),
                          # --- PERUBAHAN DI SINI: Menggunakan fluidRow nested untuk menempatkan dua tombol berdampingan ---
                          fluidRow(
                            column(6, actionButton("save_categorized_data", "Simpan Data Kategori", class = "btn btn-warning")),
                            column(6, downloadButton("download_categorized_csv", "Unduh Data Kategori (CSV)", class = "btn btn-warning"))
                          ),
                          # --- AKHIR PERUBAHAN ---
                          hr(),
                          # --- Tombol "Ulangi untuk variabel lain" ---
                          actionButton("reset_categorization", "Ulangi untuk variabel lain", class = "btn btn-warning")
                        )
                 ),
                 column(6, # Kolom kanan untuk ringkasan hasil
                        wellPanel( # wellPanel untuk memberikan latar belakang abu-abu yang terkotak
                          h3("Hasil Kategorisasi Sementara"),
                          p("Ini adalah preview ringkasan dari data yang telah dikategorikan. Klik 'Simpan Data Kategori' untuk menyimpannya."),
                          verbatimTextOutput("categorization_output_summary"),
                          h4("Interpretasi Kategorisasi:"),
                          p("Ringkasan ini menunjukkan bagaimana data asli Anda terbagi menjadi kategori-kategori numerik. Batas interval untuk setiap kategori juga ditampilkan sebagai referensi.")
                        )
                 )
               ),
               fluidRow( # Baris kedua untuk tabel pratinjau (full width)
                 column(12,
                        h3("Tampilan Data Pratinjau"),
                        p("Data berikut menunjukkan 'DISTRICTCODE', variabel asli (numerik), dan variabel kategori yang baru dibuat."),
                        DTOutput("preview_categorized_data_table") # Tabel pratinjau di sini
                 )
               )
             )
    ),
    
    # Sub-menu 3: Tabel Data Kategori
    tabPanel("Tabel Data Kategori",
             fluidRow(
               column(12,
                      h3("Data Kontinu yang Telah Dikategorikan"),
                      p("Tabel ini akan menampilkan 'DISTRICTCODE' dan semua variabel kategori yang telah Anda buat dan simpan."),
                      DTOutput("final_categorized_data_table")
               )
             )
    )
  )
)