# ui/ui_eksplorasi_data.R

ui_eksplorasi_data_tab <- fluidPage(
  h2("Eksplorasi Data"),
  
  # Baris 1: Statistik Deskriptif (Full Width)
  fluidRow(
    column(12,
           wellPanel(
             h3("Statistik Deskriptif"),
             selectInput("desc_stats_var", "Pilih Variabel Numerik:",
                         choices = numeric_cols),
             # --- Perubahan di sini: class ditambahkan ke actionButton ini ---
             actionButton("show_desc_stats", "Tampilkan Statistik", class = "btn btn-warning"),
             # --- Akhir perubahan ---
             hr(),
             verbatimTextOutput("descriptive_stats"),
             h4("Interpretasi Statistik Deskriptif:"),
             p("Bagian ini menampilkan ringkasan statistik (rata-rata, median, standar deviasi, min, maks, kuartil) untuk variabel numerik yang Anda pilih. Ini membantu memahami distribusi dan karakteristik dasar data seperti pusat, sebaran, dan keberadaan outlier.")
           )
    )
  ),
  
  # Tambahkan garis pemisah visual antara Statistik Deskriptif dan Plot
  hr(),
  
  # Baris 2: Histogram (Full Width)
  fluidRow(
    column(12,
           wellPanel(
             h3("Histogram"),
             selectInput("hist_var", "Pilih Variabel Numerik:",
                         choices = numeric_cols),
             plotOutput("variable_histogram"),
             downloadButton("download_histogram_plot", "Download Histogram (PNG)", class = "btn btn-warning"),
             h4("Interpretasi Histogram:"),
             p("Histogram menunjukkan distribusi frekuensi data. Bentuk histogram dapat mengindikasikan apakah data cenderung berdistribusi normal, miring ke kiri (left-skewed), miring ke kanan (right-skewed), atau memiliki lebih dari satu puncak (bimodal/multimodal).")
           )
    )
  ),
  
  # Tambahkan garis pemisah visual antara Histogram dan Box Plot
  hr(),
  
  # Baris 3: Box Plot (Full Width)
  fluidRow(
    column(12,
           wellPanel(
             h3("Box Plot"),
             selectInput("boxplot_num_var", "Pilih Variabel Numerik (Y-axis):",
                         choices = numeric_cols),
             uiOutput("boxplot_cat_var_ui"), # UI Output untuk variabel kategorik (dinamis)
             plotOutput("variable_boxplot"),
             downloadButton("download_boxplot_plot", "Download Box Plot (PNG)", class = "btn btn-warning"),
             h4("Interpretasi Box Plot:"),
             p("Box plot menggambarkan median (garis tengah), kuartil (kotak), dan rentang data (whiskers). Titik di luar whiskers adalah outlier. Jika variabel kategorik dipilih, box plot akan menunjukkan perbandingan distribusi variabel numerik antar kategori.")
           )
    )
  ),
  
  # Tambahkan garis pemisah visual antara Box Plot dan Scatter Plot
  hr(),
  
  # Baris 4: Scatter Plot (Full Width)
  fluidRow(
    column(12,
           wellPanel(
             h3("Scatter Plot"),
             selectInput("scatter_x_var", "Pilih Variabel Numerik (X-axis):",
                         choices = numeric_cols),
             selectInput("scatter_y_var", "Pilih Variabel Numerik (Y-axis):",
                         choices = numeric_cols, selected = numeric_cols[min(2, length(numeric_cols))]),
             plotOutput("variable_scatterplot"),
             downloadButton("download_scatterplot_plot", "Download Scatter Plot (PNG)", class = "btn btn-warning"),
             h4("Interpretasi Scatter Plot:"),
             p("Scatter plot menunjukkan hubungan atau korelasi antara dua variabel numerik. Pola titik-titik dapat mengindikasikan korelasi positif (naik), negatif (turun), atau tidak ada korelasi (menyebar acak).")
           )
    )
  ),
  
  # --- MODIFIKASI BAGIAN PETA DI SINI ---
  hr(), # Garis pemisah visual sebelum peta
  fluidRow(
    column(12,
           wellPanel(
             h3("Peta Tematik Distrik Indonesia"), # Judul yang lebih spesifik
             p("Peta ini menampilkan distribusi spasial dari variabel yang dipilih berdasarkan distrik."),
             uiOutput("map_var_select_ui"), # UI Output baru untuk pilihan variabel peta
             leafletOutput("sovi_map", height = "600px"), # Output peta
             h4("Interpretasi Peta:"),
             p("Peta koroplet menunjukkan pola spasial variabel yang dipilih. Area yang diwarnai lebih gelap atau lebih terang mengindikasikan nilai yang lebih tinggi atau lebih rendah dari variabel tersebut di distrik-distrik yang berbeda. Hover (arahkan kursor) atau klik pada distrik untuk melihat detail.")
           )
    )
  ),
  # --- AKHIR MODIFIKASI BAGIAN PETA ---
  
  hr(), # Garis pemisah visual sebelum download laporan
  fluidRow(
    column(12,
           wellPanel(
             h3("Download Laporan Eksplorasi Data"),
             p("Unduh laporan lengkap yang mencakup statistik deskriptif, plot, dan interpretasinya dalam format RMarkdown (.Rmd). User harus mengkategorikan data
                terlebih dahulu untuk menggunakan fitur unduh ini dengan baik. User juga perlu mengunduh dataset awal dan data yang sudah dikategorikan yang berada 
                di menu Manajemen Data. Letakkan file Rmd Saudara di dalam satu file yang sama dengan file dataset dan data kategori untuk memudahkan dalam menjalankan 
                file Rmd yang sudah Saudara unduh. Harap tidak mengubah nama file dari dataset dan data kategori demi kenyamanan user sendiri. Terima kasih."),
             downloadButton("download_eksplorasi_report", "Download Laporan (RMD)", class = "btn btn-warning")
           )
    )
  )
)