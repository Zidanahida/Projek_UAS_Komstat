# app.R

# --- Memuat Paket yang Diperlukan ---
library(shiny)
library(dplyr)
library(ggplot2)
library(DT) # Untuk tabel interaktif
library(car) # Untuk Levene's Test, durbinWatsonTest, ncvTest, vif
library(nortest) # Untuk ad.test (Anderson-Darling Normality Test)
library(classInt) # Untuk pengkategorian data
library(rmarkdown) # Untuk laporan RMarkdown
library(sf) # Tambahkan paket untuk data spasial (sf)
library(leaflet) # Tambahkan paket untuk peta interaktif (leaflet)
library(bslib) # <--- Tambahkan paket bslib

# --- Persiapan Data Global ---
sovi_data <- read.csv("sovi_data.csv")
numeric_cols <- names(sovi_data)[sapply(sovi_data, is.numeric)]
numeric_cols <- numeric_cols[numeric_cols != "DISTRICTCODE"]

# --- Pemuatan Data GeoJSON dan Penggabungan Statistik ---
# map_data_base_geojson akan berisi data sf dari GeoJSON yang lebih ringan
map_data_base_geojson <- tryCatch({
  st_read("indonesia_simplified.geojson", quiet = TRUE) # Menggunakan file GeoJSON yang lebih ringan
}, error = function(e) {
  message("Error loading indonesia_simplified.geojson: ", e$message)
  NULL
})

# map_data_joined_for_map akan menjadi hasil akhir join GeoJSON dengan sovi_data.csv
map_data_joined_for_map <- NULL

if (is.null(map_data_base_geojson)) {
  warning("Spatial data (indonesia_simplified.geojson) could not be loaded. Map functionality will be limited or unavailable.")
  shiny::onStop(function() {
    showNotification("Error: File 'indonesia_simplified.geojson' tidak ditemukan atau tidak dapat dimuat. Pastikan file berada di direktori kerja aplikasi.", type = "error", duration = NULL)
  })
} else {
  # === PENTING: Lakukan rename kodeprkab ke DISTRICTCODE di GeoJSON ===
  if ("kodeprkab" %in% names(map_data_base_geojson)) {
    map_data_base_geojson <- map_data_base_geojson %>% rename(DISTRICTCODE = kodeprkab)
  } else if ("kddistrict" %in% names(map_data_base_geojson)) { # Fallback
    map_data_base_geojson <- map_data_base_geojson %>% rename(DISTRICTCODE = kddistrict)
    warning("Column 'kodeprkab' not found in GeoJSON. Using 'kddistrict' as join key for map.")
  } else {
    warning("Neither 'kodeprkab' nor 'kddistrict' found in GeoJSON. Cannot perform spatial join correctly.")
    map_data_base_geojson <- NULL # Set to NULL if no valid join key
  }
  
  if (!is.null(map_data_base_geojson)) {
    # Pastikan DISTRICTCODE di kedua data bertipe karakter untuk join yang benar
    map_data_base_geojson$DISTRICTCODE <- as.character(map_data_base_geojson$DISTRICTCODE)
    sovi_data$DISTRICTCODE <- as.character(sovi_data$DISTRICTCODE)
    
    # === PENTING: Lakukan left_join di sini untuk menggabungkan data statistik ===
    map_data_joined_for_map <- map_data_base_geojson %>%
      left_join(sovi_data, by = "DISTRICTCODE")
    
    if (nrow(map_data_joined_for_map) == 0 || !("CHILDREN" %in% names(map_data_joined_for_map))) {
      warning("Joined map data is empty or missing expected statistical columns after join. Check join keys and data integrity.")
      map_data_joined_for_map <- NULL # Set to NULL if join failed to produce meaningful data
    }
    
  } else {
    map_data_joined_for_map <- NULL # Set to NULL if base geojson is problematic
  }
}
# --- Akhir pemuatan dan penggabungan data GeoJSON ---


# --- Sumber File UI Terpisah ---
source("ui/ui_beranda.R", local = TRUE)
source("ui/ui_manajemen_data.R", local = TRUE)
source("ui/ui_eksplorasi_data.R", local = TRUE)
source("ui/ui_uji_asumsi_data.R", local = TRUE)
source("ui/ui_statistik_inferensia.R", local = TRUE)
source("ui/ui_regresi_linear_berganda.R", local = TRUE)


# --- Definisi Tema bslib untuk navbarPage ---
my_navbar_theme <- bs_theme(
  version = 4,
  bootswatch = "cerulean",
  bg = "#F8F9FA", # Background
  fg = "#212529",
  primary = "#007BFF",
  secondary = "orange",
  base_font = font_google("Roboto"),
  heading_font = font_google("Montserrat")
)


# --- Definisi User Interface (UI) Utama (dengan bslib di navbarPage) ---
ui <- navbarPage(
  "DAD'S",
  theme = my_navbar_theme, # <--- Terapkan tema bslib di sini
  
  tabPanel("Beranda", ui_beranda_tab),
  tabPanel("Manajemen Data", ui_manajemen_data_tab),
  tabPanel("Eksplorasi Data", ui_eksplorasi_data_tab),
  tabPanel("Uji Asumsi Data", ui_uji_asumsi_data_tab),
  tabPanel("Statistik Inferensia", ui_statistik_inferensia_tab),
  tabPanel("Regresi Linear Berganda", ui_regresi_linear_berganda_tab)
)

# --- Definisi Server Logic ---
server <- function(input, output, session) {
  
  # ... (Semua kode server logic Anda sebelumnya, dari stored_categorized_data hingga downloadHandler, tetap di sini) ...
  
  # Reactive Value untuk menyimpan data kategori yang 'disimpan'
  stored_categorized_data <- reactiveVal(sovi_data %>% select(DISTRICTCODE))
  
  # Reactive expression untuk hasil kategorisasi sementara (preview di transformasi data)
  preview_categorized_data_info <- eventReactive(input$categorize_btn, {
    req(input$var_to_categorize, input$num_breaks, input$categorization_method)
    
    original_var <- sovi_data[[input$var_to_categorize]]
    num_breaks <- input$num_breaks
    method <- input$categorization_method
    
    ci <- classInt::classIntervals(original_var, n = num_breaks, style = method)
    categorized_var_factor <- cut(original_var, breaks = ci$brks, include.lowest = TRUE, right = TRUE)
    categorized_var_numeric <- as.integer(categorized_var_factor)
    
    new_col_name <- paste0(
      input$var_to_categorize, "_",
      toupper(substr(input$categorization_method, 1, 1)),
      substr(input$categorization_method, 2, nchar(input$categorization_method)),
      "_", input$num_breaks, "K"
    )
    
    new_col_name <- gsub("[^a-zA-Z0-9_]", "", new_col_name)
    
    data_for_preview <- data.frame(
      DISTRICTCODE = sovi_data$DISTRICTCODE,
      Original_Var = original_var
    )
    names(data_for_preview)[names(data_for_preview) == "Original_Var"] <- input$var_to_categorize
    data_for_preview[[new_col_name]] <- categorized_var_numeric
    
    list(
      data = data_for_preview,
      summary_original = summary(original_var),
      summary_categorical_table = table(categorized_var_numeric),
      breaks = ci$brks,
      full_labels = levels(categorized_var_factor)
    )
  })
  
  # --- Beranda Server ---
  output$variable_description_table <- renderDT({
    # Data untuk tabel deskripsi variabel
    variable_desc_data <- data.frame(
      Variabel = c("DISTRICTCODE","CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE",
                   "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE",
                   "NOTRAINING", "DPRONE", "RENTED", "NOSEWER", "TAPWATER",
                   "POPULATION"),
      Nama = c("Kode Kota/Kabupaten","Anak-anak", "Perempuan", "Lansia", "Kepala RT Perempuan", "Ukuran Keluarga",
               "Tanpa Listrik", "Pendidikan Rendah", "Pertumbuhan", "Kemiskinan", "Buta Huruf",
               "Tanpa Pelatihan", "Rawan Bencana", "Sewa Rumah", "Tanpa Drainase", "Air Keran",
               "Populasi"),
      Deskripsi = c("Kode Kota/Kabupaten seluruh Indonesia menurut BPS",
                    "Persentase penduduk usia di bawah lima tahun",
                    "Persentase penduduk perempuan",
                    "Persentase penduduk usia 65 tahun ke atas",
                    "Persentase rumah tangga dengan kepala rumah tangga perempuan",
                    "Rata-rata jumlah anggota rumah tangga",
                    "Persentase rumah tangga yang tidak menggunakan listrik",
                    "Persentase penduduk usia 15+ dengan pendidikan rendah",
                    "Persentase perubahan populasi",
                    "Persentase penduduk miskin",
                    "Persentase penduduk yang tidak bisa membaca dan menulis",
                    "Persentase rumah tangga tanpa pelatihan bencana",
                    "Persentase rumah tangga di daerah rawan bencana",
                    "Persentase rumah tangga yang menyewa rumah",
                    "Persentase rumah tangga tanpa sistem drainase",
                    "Persentase rumah tangga yang menggunakan air keran",
                    "Jumlah penduduk")
    )
    datatable(variable_desc_data, options = list(pageLength = 17, dom = 't', scrollX = TRUE)) # dom = 't' menghilangkan search/pagination, scrollX untuk tabel lebar
  }, server = FALSE)
  
  # --- Manajemen Data Server ---
  
  output$full_sovi_data_table <- renderDT({
    datatable(sovi_data, options = list(pageLength = 10, scrollX = TRUE), filter = 'top')
  })
  
  output$categorization_output_summary <- renderPrint({
    if (input$categorize_btn == 0) return("Pilih variabel dan metode untuk mengkategorikan data.")
    info <- preview_categorized_data_info()
    
    cat("Ringkasan Data Asli untuk", input$var_to_categorize, ":\n")
    print(info$summary_original)
    
    cat("\nBatas Interval Kategori (untuk referensi):\n")
    cat("Intervals:\n")
    print(info$breaks)
    cat("\nLabel Interval Penuh:\n")
    print(info$full_labels)
  })
  
  output$preview_categorized_data_table <- renderDT({
    if (input$categorize_btn == 0) return(NULL)
    datatable(preview_categorized_data_info()$data,
              options = list(pageLength = 10, scrollX = TRUE),
              filter = 'top')
  })
  
  observeEvent(input$save_categorized_data, {
    info <- preview_categorized_data_info()
    if (!is.null(info$data)) {
      new_col_data <- info$data %>% select(DISTRICTCODE, last_col())
      
      current_stored_data <- stored_categorized_data()
      new_col_name <- names(new_col_data)[2]
      
      if (new_col_name %in% names(current_stored_data)) {
        current_stored_data[[new_col_name]] <- new_col_data[[new_col_name]]
        stored_categorized_data(current_stored_data)
        showNotification(paste0("Variabel '", new_col_name, "' berhasil diperbarui!"), type = "message")
      } else {
        updated_data <- left_join(current_stored_data, new_col_data, by = "DISTRICTCODE")
        stored_categorized_data(updated_data)
        showNotification(paste0("Variabel '", new_col_name, "' berhasil ditambahkan!"), type = "message")
      }
      # Save categorized data to CSV so it can be accessed by the Rmd if needed
      write.csv(stored_categorized_data(), "categorized_data.csv", row.names = FALSE)
    } else {
      showNotification("Tidak ada data kategori untuk disimpan. Silakan proses data terlebih dahulu.", type = "warning")
    }
  })
  
  observeEvent(input$reset_categorization, {
    showNotification("Siap untuk mengkategorikan variabel lain!", type = "message")
  })
  
  output$final_categorized_data_table <- renderDT({
    current_data <- stored_categorized_data()
    if (ncol(current_data) > 1) {
      datatable(current_data,
                options = list(pageLength = 10, scrollX = TRUE),
                filter = 'top')
    } else {
      data.frame(Pesan = "Belum ada variabel kategori yang disimpan. Silakan proses data di 'Transformasi Data' dan klik 'Simpan Data Kategori'.") %>%
        datatable(options = list(dom = 't'))
    }
  })
  
  # --- Tambahkan atau pastikan block kode ini ada di server Anda ---
  # Download Handler untuk Data Kategori (CSV)
  output$download_categorized_csv <- downloadHandler(
    filename = function() {
      paste("categorized_data",".csv", sep = "")
    },
    content = function(file) {
      # Periksa apakah ada kolom kategori selain DISTRICTCODE
      if (ncol(stored_categorized_data()) > 1) {
        write.csv(stored_categorized_data(), file, row.names = FALSE)
      } else {
        # Jika tidak ada data kategori, berikan notifikasi
        showNotification(
          "Tidak ada variabel kategori yang disimpan untuk diunduh. Silakan proses data di 'Transformasi Data' dan klik 'Simpan Data Kategori' terlebih dahulu.",
          type = "warning",
          duration = 5
        )
        # Menghentikan proses download (penting agar browser tidak mencoba mendownload file kosong)
        stop("Tidak ada data kategori yang tersimpan.")
      }
    }
  )
  # --- Akhir block kode yang perlu ditambahkan/dipastikan ---
  
  # --- Reactive Global untuk Variabel Kategorik (digunakan di Eksplorasi & Inferensia) ---
  all_categorical_vars_reactive <- reactive({
    original_pseudo_cat_cols <- names(sovi_data)[sapply(sovi_data, function(x) {
      is.numeric(x) && length(unique(x)) < 10 && length(unique(x)) > 1
    })]
    
    transformed_cat_cols <- names(stored_categorized_data())
    transformed_cat_cols <- transformed_cat_cols[transformed_cat_cols != "DISTRICTCODE"]
    
    choices <- c(transformed_cat_cols, original_pseudo_cat_cols)
    if (length(choices) == 0) {
      return(c("Tidak Ada Variabel Kategori Tersedia"))
    }
    unique(choices)
  })
  
  # --- Eksplorasi Data Server ---
  
  output$boxplot_cat_var_ui <- renderUI({
    choices <- c("None", all_categorical_vars_reactive())
    selectInput("boxplot_cat_var", "Pilih Variabel Kategorik (X-axis):",
                choices = choices,
                selected = "None")
  })
  
  output$descriptive_stats <- renderPrint({
    req(input$show_desc_stats)
    req(input$desc_stats_var)
    summary(sovi_data[[input$desc_stats_var]])
  })
  
  output$variable_histogram <- renderPlot({
    req(input$hist_var)
    ggplot(sovi_data, aes_string(x = input$hist_var)) +
      geom_histogram(binwidth = diff(range(sovi_data[[input$hist_var]], na.rm = TRUE))/30, fill = "steelblue", color = "black") +
      labs(title = paste("Histogram", input$hist_var), x = input$hist_var, y = "Frekuensi") +
      theme_minimal()
  })
  
  output$variable_boxplot <- renderPlot({
    req(input$boxplot_num_var)
    
    if (input$boxplot_cat_var == "None") {
      ggplot(sovi_data, aes_string(y = input$boxplot_num_var)) +
        geom_boxplot(fill = "lightblue") +
        labs(title = paste("Box Plot", input$boxplot_num_var), y = input$boxplot_num_var) +
        theme_minimal() +
        coord_flip()
    } else {
      plot_data <- left_join(sovi_data, stored_categorized_data(), by = "DISTRICTCODE")
      
      if (!(input$boxplot_cat_var %in% names(plot_data))) {
        warning("Variabel kategorik yang dipilih tidak ditemukan setelah penggabungan data.")
        return(ggplot() + labs(title = "Variabel kategori tidak ditemukan.") + theme_void())
      }
      
      num_var <- plot_data[[input$boxplot_num_var]]
      cat_var <- plot_data[[input$boxplot_cat_var]]
      
      valid_indices <- !is.na(num_var) & !is.na(cat_var)
      plot_data_filtered <- plot_data[valid_indices, ]
      
      if (nrow(plot_data_filtered) == 0) {
        return(ggplot() + labs(title = "Tidak ada data yang valid untuk plot boxplot setelah pembersihan NA.") + theme_void())
      }
      
      plot_data_filtered[[input$boxplot_cat_var]] <- as.factor(plot_data_filtered[[input$boxplot_cat_var]])
      
      if (nlevels(plot_data_filtered[[input$boxplot_cat_var]]) < 2) {
        ggplot(plot_data_filtered, aes_string(y = input$boxplot_num_var)) +
          geom_boxplot(fill = "lightblue") +
          labs(title = paste("Box Plot", input$boxplot_num_var, "(Variabel Kategorik hanya memiliki 1 level/data tidak cukup)"), y = input$boxplot_num_var) +
          theme_minimal() +
          coord_flip()
      } else {
        ggplot(plot_data_filtered, aes_string(x = input$boxplot_cat_var, y = input$boxplot_num_var, fill = input$boxplot_cat_var)) +
          geom_boxplot() +
          labs(title = paste("Box Plot", input$boxplot_num_var, "berdasarkan", input$boxplot_cat_var),
               x = input$boxplot_cat_var, y = input$boxplot_num_var) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    }
  })
  
  output$variable_scatterplot <- renderPlot({
    req(input$scatter_x_var, input$scatter_y_var)
    ggplot(sovi_data, aes_string(x = input$scatter_x_var, y = input$scatter_y_var)) +
      geom_point(alpha = 0.6) +
      labs(title = paste("Scatter Plot", input$scatter_x_var, "vs", input$scatter_y_var),
           x = input$scatter_x_var, y = input$scatter_y_var) +
      theme_minimal()
  })
  
  # --- LOGIKA SERVER UNTUK PETA DI SINI ---
  # Reactive untuk daftar variabel numerik yang tersedia di GeoJSON (untuk pilihan peta)
  # map_numeric_vars_reactive akan mengambil kolom dari map_data_joined_for_map
  map_numeric_vars_reactive <- reactive({
    if (is.null(map_data_joined_for_map)) { # Cek data yang sudah digabungkan
      return(c("Tidak Ada Variabel Data Peta"))
    }
    # Ambil nama-nama kolom numerik dari atribut GeoJSON yang sudah digabungkan (selain geometry dan ID)
    geo_attributes <- st_drop_geometry(map_data_joined_for_map)
    numeric_geo_cols <- names(geo_attributes)[sapply(geo_attributes, is.numeric)]
    # Filter out FID, gid, dan kolom ID spasial lainnya yang tidak ingin dipetakan
    # Termasuk 'kdkab', 'kdprov' jika itu ID internal dan bukan data untuk dipetakan
    numeric_geo_cols <- numeric_geo_cols[! numeric_geo_cols %in% c("FID", "gid", "kdkab", "kdprov")]
    
    if (length(numeric_geo_cols) == 0) {
      return(c("Tidak Ada Variabel Numerik di Peta"))
    }
    unique(numeric_geo_cols)
  })
  
  # UI Dinamis untuk pilihan variabel peta
  output$map_var_select_ui <- renderUI({
    choices <- map_numeric_vars_reactive()
    selectInput("map_var_select_ui_id", # ID unik untuk input ini
                "Pilih Variabel Numerik untuk Dipetakan:",
                choices = choices,
                selected = if ("POPULATION" %in% choices) "POPULATION" else choices[1]) # Default ke POPULATION atau pilihan pertama
  })
  
  # Reactive untuk data peta yang akan ditampilkan (hasil join)
  map_data_reactive <- reactive({
    # Tidak lagi req(input$map_var_select_ui_id) di sini agar peta dasar bisa muncul duluan
    
    if (is.null(map_data_joined_for_map)) { # Cek data yang sudah digabungkan
      showNotification("Data geografis dan statistik tidak dapat digabungkan. Peta tidak tersedia.", type = "error", duration = 5)
      return(NULL)
    }
    
    base_map_data <- map_data_joined_for_map # Sekarang ini adalah data hasil join
    
    selected_var <- input$map_var_select_ui_id
    if (is.null(selected_var) || selected_var == "Tidak Ada Variabel Data Peta" || selected_var == "Tidak Ada Variabel Numerik di Peta") {
      default_choices <- map_numeric_vars_reactive()
      if (length(default_choices) > 0 && default_choices[1] != "Tidak Ada Variabel Data Peta") {
        selected_var <- default_choices[1]
      } else {
        selected_var <- NULL
      }
    }
    
    if (!is.null(selected_var) && (selected_var %in% names(st_drop_geometry(base_map_data)))) {
      map_data_to_plot <- base_map_data %>%
        filter(!is.na(!!sym(selected_var)))
      if (nrow(map_data_to_plot) == 0) {
        showNotification(paste0("Tidak ada data valid untuk '", selected_var, "' di peta setelah pembersihan missing values."), type = "warning", duration = 5)
        return(NULL)
      }
      return(map_data_to_plot)
    } else {
      return(base_map_data) # Kembalikan data gabungan dasar jika tidak ada variabel valid dipilih
    }
  })
  
  output$sovi_map <- renderLeaflet({
    req(map_data_reactive())
    
    data_for_map <- map_data_reactive()
    selected_var <- input$map_var_select_ui_id
    
    is_thematic_possible <- FALSE
    pal <- NULL
    labels_to_use <- NULL
    
    if (!is.null(selected_var) &&
        selected_var != "Tidak Ada Variabel Data Peta" &&
        selected_var != "Tidak Ada Variabel Numerik di Peta" &&
        (selected_var %in% names(st_drop_geometry(data_for_map)))) {
      
      if (length(unique(data_for_map[[selected_var]][!is.na(data_for_map[[selected_var]])])) > 1) {
        is_thematic_possible <- TRUE
        pal <- colorNumeric(
          palette = "YlOrRd",
          domain = data_for_map[[selected_var]]
        )
        labels_to_use <- paste0(
          "<strong>Distrik:</strong> ", data_for_map$nmkab, " (", data_for_map$DISTRICTCODE, ")<br/>",
          "<strong>Provinsi:</strong> ", data_for_map$nmprov, "<br/>",
          "<strong>", selected_var, ":</strong> ", formatC(data_for_map[[selected_var]], format = "f", digits = 2)
        ) %>% lapply(htmltools::HTML)
      } else {
        showNotification(paste0("Variabel '", selected_var, "' tidak memiliki variasi nilai yang cukup untuk peta tematik."), type = "warning", duration = 5)
      }
    }
    
    if (!is_thematic_possible) {
      labels_to_use <- paste0(
        "<strong>Distrik:</strong> ", data_for_map$nmkab, " (", data_for_map$DISTRICTCODE, ")<br/>",
        "<strong>Provinsi:</strong> ", data_for_map$nmprov
      ) %>% lapply(htmltools::HTML)
    }
    
    if (nrow(data_for_map) == 0 || !("sf" %in% class(data_for_map))) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = 118, lat = -2.5, zoom = 5) %>%
        addLabelOnlyMarkers(lng = 118, lat = -2.5, label = "Tidak ada data untuk menampilkan peta.")
    } else {
      m <- leaflet(data_for_map) %>%
        addTiles() %>%
        addPolygons(
          fillColor = if (is_thematic_possible) pal(data_for_map[[selected_var]]) else "lightblue",
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 3,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels_to_use,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto")
        )
      
      if (is_thematic_possible) {
        m <- m %>% addLegend(pal = pal, values = data_for_map[[selected_var]], opacity = 0.7,
                             title = selected_var, position = "bottomright")
      }
      m
    }
  })
  
  # --- Uji Asumsi Data Server ---
  output$homogeneity_cat_var_ui <- renderUI({
    choices <- c("Pilih Kategori", all_categorical_vars_reactive())
    selectInput("homogeneity_cat_var", "Pilih Variabel Kategorik (Grup):",
                choices = choices,
                selected = "Pilih Kategori")
  })
  
  # Implementasi observeEvent untuk Uji Normalitas (tetap ada)
  observeEvent(input$run_normality_test, {
    req(input$normality_var)
    selected_var_data <- sovi_data[[input$normality_var]]
    
    if (any(is.na(selected_var_data))) {
      output$normality_test_output <- renderPrint({
        cat("Error: Data untuk variabel", input$normality_var, "mengandung missing value (NA). Uji normalitas tidak dapat dilakukan.\n")
      })
      return()
    }
    if (length(unique(selected_var_data)) < 5) {
      output$normality_test_output <- renderPrint({
        cat("Error: Variabel", input$normality_var, "tidak memiliki cukup nilai unik (minimal 5) untuk uji normalitas.\n")
      })
      return()
    }
    
    norm_test_result <- ad.test(selected_var_data)
    output$normality_test_output <- renderPrint({
      cat("Hasil Uji Normalitas (Anderson-Darling Test) untuk", input$normality_var, ":\n")
      print(norm_test_result)
      cat("\nInterpretasi:\n")
      if (norm_test_result$p.value < 0.05) {
        cat("Nilai p (", format(round(norm_test_result$p.value, 4), scientific = FALSE), ") < 0.05.\n")
        cat("Karena nilai p < tingkat signifikansi, kita menolak Hipotesis Nol (H0).\n")
        cat("Ini berarti ada bukti statistik yang kuat bahwa data ", input$normality_var, " TIDAK berdistribusi normal.\n")
      } else {
        cat("Nilai p (", format(round(norm_test_result$p.value, 4), scientific = FALSE), ") >= 0.05.\n")
        cat("Karena nilai p >= tingkat signifikansi, kita GAGAL menolak Hipotesis Nol (H0).\n")
        cat("Ini berarti tidak ada cukup bukti statistik untuk menyimpulkan bahwa data ", input$normality_var, " berdistribusi normal.\n")
      }
    })
  })
  
  # Implementasi observeEvent untuk Uji Homogenitas Varians (tetap ada)
  observeEvent(input$run_homogeneity_test, {
    req(input$homogeneity_num_var)
    req(input$homogeneity_cat_var)
    
    if (input$homogeneity_cat_var == "Pilih Kategori") {
      output$homogeneity_test_output <- renderPrint({
        cat("Silakan pilih variabel kategorik (grup) untuk menjalankan Uji Homogenitas Varians.")
      })
      return()
    }
    
    selected_num_data <- sovi_data[[input$homogeneity_num_var]]
    
    if (input$homogeneity_cat_var %in% names(stored_categorized_data())) {
      group_var_data <- stored_categorized_data()[[input$homogeneity_cat_var]]
    } else {
      group_var_data <- sovi_data[[input$homogeneity_cat_var]]
      if (!is.factor(group_var_data) && length(unique(group_var_data)) > 10) {
        showNotification("Variabel grup yang dipilih memiliki banyak nilai unik dan bukan faktor. Ini akan dikategorikan secara otomatis berdasarkan kuartil untuk tujuan uji homogenitas.", type = "warning", duration = 5)
        group_var_data <- cut(group_var_data, breaks = unique(quantile(group_var_data, probs = seq(0, 1, by = 0.25), na.rm = TRUE)), include.lowest = TRUE, dig.lab = 4)
      }
    }
    group_var_data <- as.factor(group_var_data)
    
    temp_df_homo <- data.frame(val = selected_num_data, group = group_var_data)
    temp_df_homo <- na.omit(temp_df_homo)
    
    if (nrow(temp_df_homo) == 0) {
      output$homogeneity_test_output <- renderPrint({
        cat("Tidak ada data valid untuk Uji Homogenitas setelah menghilangkan missing values atau variabel grup tidak memiliki cukup data.")
      })
      return()
    }
    
    if (length(unique(temp_df_homo$group)) < 2) {
      output$homogeneity_test_output <- renderPrint({
        cat("Variabel grup yang dipilih hanya memiliki satu kategori unik setelah pembersihan data. Uji homogenitas memerlukan setidaknya dua kelompok.")
      })
      return()
    }
    
    levene_test_result <- leveneTest(temp_df_homo$val ~ temp_df_homo$group)
    output$homogeneity_test_output <- renderPrint({
      cat("Hasil Uji Homogenitas Varians (Levene's Test) untuk", input$homogeneity_num_var, "berdasarkan grup", input$homogeneity_cat_var, ":\n")
      print(levene_test_result)
      cat("\nInterpretasi:\n")
      if (levene_test_result$`Pr(>F)`[1] < 0.05) {
        cat("Nilai p (", format(round(levene_test_result$`Pr(>F)`[1], 4), scientific = FALSE), ") < 0.05.\n")
        cat("Karena nilai p < tingkat signifikansi, kita menolak Hipotesis Nol (H0).\n")
        cat("Ini berarti ada bukti statistik yang kuat bahwa varians variabel ", input$homogeneity_num_var, " antar kelompok ", input$homogeneity_cat_var, " TIDAK homogen.\n")
      } else {
        cat("Nilai p (", format(round(levene_test_result$`Pr(>F)`[1], 4), scientific = FALSE), ") >= 0.05.\n")
        cat("Karena nilai p >= tingkat signifikansi, kita GAGAL menolak Hipotesis Nol (H0).\n")
        cat("Ini berarti tidak ada cukup bukti statistik untuk menyimpulkan bahwa varians variabel ", input$homogeneity_num_var, " antar kelompok ", input$homogeneity_cat_var, " homogen.\n")
      }
    })
  })
  
  # --- Statistik Inferensia Server ---
  
  output$two_sample_t_cat_var_ui <- renderUI({
    choices <- c("Pilih Kategori", all_categorical_vars_reactive())
    selectInput("two_sample_t_cat_var", "Pilih Variabel Kategorik (Grup, 2 level):",
                choices = choices,
                selected = "Pilih Kategori")
  })
  
  output$var_test_cat_var_ui <- renderUI({
    choices <- c("Pilih Kategori", all_categorical_vars_reactive())
    selectInput("var_test_cat_var", "Pilih Variabel Kategorik (Grup, 2 level):",
                choices = choices,
                selected = "Pilih Kategori")
  })
  
  output$anova_1w_cat_var_ui <- renderUI({
    choices <- c("Pilih Kategori", all_categorical_vars_reactive())
    selectInput("anova_1w_cat_var", "Pilih Variabel Kategorik (Independent Factor):",
                choices = choices,
                selected = "Pilih Kategori")
  })
  
  output$anova_2w_cat_var1_ui <- renderUI({
    choices <- c("Pilih Kategori", all_categorical_vars_reactive())
    selectInput("anova_2w_cat_var1", "Pilih Variabel Kategorik (Faktor 1):",
                choices = choices,
                selected = "Pilih Kategori")
  })
  
  output$anova_2w_cat_var2_ui <- renderUI({
    choices <- c("Pilih Kategori", all_categorical_vars_reactive())
    selectInput("anova_2w_cat_var2", "Pilih Variabel Kategorik (Faktor 2):",
                choices = choices,
                selected = "Pilih Kategori")
  })
  
  
  observeEvent(input$run_one_sample_t, {
    req(input$one_sample_t_var, input$one_sample_t_mu, input$one_sample_t_alt)
    data_var <- sovi_data[[input$one_sample_t_var]]
    data_var <- na.omit(data_var)
    
    if (length(data_var) < 2) {
      output$one_sample_t_output <- renderPrint("Error: Variabel tidak memiliki cukup data (minimal 2 observasi setelah menghilangkan NA).")
      output$one_sample_t_interpret <- renderPrint("")
      return()
    }
    
    result <- t.test(data_var, mu = input$one_sample_t_mu, alternative = input$one_sample_t_alt)
    output$one_sample_t_output <- renderPrint({
      print(result)
    })
    output$one_sample_t_interpret <- renderText({
      p_val <- result$p.value
      h0_text <- paste0("Rata-rata populasi (μ) sama dengan ", input$one_sample_t_mu, ".")
      h1_text <- switch(input$one_sample_t_alt,
                        "two.sided" = paste0("Rata-rata populasi (μ) tidak sama dengan ", input$one_sample_t_mu, "."),
                        "less" = paste0("Rata-rata populasi (μ) kurang dari ", input$one_sample_t_mu, "."),
                        "greater" = paste0("Rata-rata populasi (μ) lebih dari ", input$one_sample_t_mu, "."))
      
      interpret <- paste0(
        "Hipotesis Nol (H0): ", h0_text, "\n",
        "Hipotesis Alternatif (H1): ", h1_text, "\n",
        "Nilai p (p-value): ", format(round(p_val, 4), scientific = FALSE), ".\n"
      )
      
      if (p_val < 0.05) {
        interpret <- paste0(interpret,
                            "Karena nilai p < 0.05, kita menolak H0.\n",
                            "Ini berarti ada bukti statistik yang signifikan untuk menyimpulkan bahwa ", h1_text, " pada tingkat signifikansi 5%.")
      } else {
        interpret <- paste0(interpret,
                            "Karena nilai p >= 0.05, kita gagal menolak H0.\n",
                            "Ini berarti tidak ada cukup bukti statistik untuk menyimpulkan bahwa ", h1_text, " pada tingkat signifikansi 5%.")
      }
      interpret
    })
  })
  
  observeEvent(input$run_two_sample_t, {
    req(input$two_sample_t_num_var, input$two_sample_t_cat_var, input$two_sample_t_type, input$two_sample_t_alt)
    
    if (input$two_sample_t_cat_var == "Pilih Kategori") {
      output$two_sample_t_output <- renderPrint("Silakan pilih variabel kategorik untuk uji 2 kelompok.")
      output$two_sample_t_interpret <- renderPrint("")
      return()
    }
    
    plot_data <- left_join(sovi_data, stored_categorized_data(), by = "DISTRICTCODE")
    
    num_var <- plot_data[[input$two_sample_t_num_var]]
    cat_var <- plot_data[[input$two_sample_t_cat_var]]
    
    if (!is.factor(cat_var)) {
      cat_var <- as.factor(cat_var)
    }
    if (nlevels(cat_var) != 2) {
      output$two_sample_t_output <- renderPrint("Error: Variabel kategorik harus memiliki tepat 2 level/kelompok untuk uji t 2 kelompok.")
      output$two_sample_t_interpret <- renderPrint("")
      return()
    }
    
    valid_data <- data.frame(num = num_var, cat = cat_var)
    valid_data <- na.omit(valid_data)
    
    if (nrow(valid_data) < 2) {
      output$two_sample_t_output <- renderPrint("Error: Variabel tidak memiliki cukup data setelah menghilangkan NA dan memfilter 2 kelompok.")
      output$two_sample_t_interpret <- renderPrint("")
      return()
    }
    
    if (any(table(valid_data$cat) < 1)) {
      output$two_sample_t_output <- renderPrint("Error: Setidaknya satu kelompok memiliki kurang dari 1 observasi setelah pembersihan data.")
      output$two_sample_t_interpret <- renderPrint("")
      return()
    }
    
    
    test_type <- input$two_sample_t_type
    paired_test <- (test_type == "paired")
    var_equal <- (test_type == "independent_equal_var")
    
    result <- NULL
    if (paired_test) {
      output$two_sample_t_output <- renderPrint("Error: Uji t berpasangan memerlukan data yang secara eksplisit berpasangan (misal: pengukuran sebelum dan sesudah pada individu yang sama). Dataset ini mungkin tidak cocok untuk uji berpasangan tanpa identifikasi pasangan yang jelas.")
      output$two_sample_t_interpret <- renderPrint("")
      return()
      
    } else {
      result <- t.test(valid_data$num ~ valid_data$cat, var.equal = var_equal, alternative = input$two_sample_t_alt)
    }
    
    if (!is.null(result)) {
      output$two_sample_t_output <- renderPrint({
        print(result)
      })
      output$two_sample_t_interpret <- renderText({
        p_val <- result$p.value
        group_names <- levels(valid_data$cat)
        h0_text <- paste0("Rata-rata populasi ", group_names[1], " sama dengan rata-rata populasi ", group_names[2], ".")
        h1_text <- switch(input$two_sample_t_alt,
                          "two.sided" = paste0("Rata-rata populasi ", group_names[1], " tidak sama dengan rata-rata populasi ", group_names[2], "."),
                          "less" = paste0("Rata-rata populasi ", group_names[1], " kurang dari rata-rata populasi ", group_names[2], "."),
                          "greater" = paste0("Rata-rata populasi ", group_names[1], " lebih dari rata-rata populasi ", group_names[2], "."))
        
        interpret <- paste0(
          "Hipotesis Nol (H0): ", h0_text, "\n",
          "Hipotesis Alternatif (H1): ", h1_text, "\n",
          "Jenis Uji t-test: ", input$two_sample_t_type, ".\n",
          "Nilai p (p-value): ", format(round(p_val, 4), scientific = FALSE), ".\n"
        )
        
        if (p_val < 0.05) {
          interpret <- paste0(interpret,
                              "Karena nilai p < 0.05, kita menolak H0.\n",
                              "Ini berarti ada bukti statistik yang signifikan untuk menyimpulkan bahwa ", h1_text, " pada tingkat signifikansi 5%.")
        } else {
          interpret <- paste0(interpret,
                              "Karena nilai p >= 0.05, kita gagal menolak H0.\n",
                              "Ini berarti tidak ada cukup bukti statistik untuk menyimpulkan bahwa ", h1_text, " pada tingkat signifikansi 5%.")
        }
        interpret
      })
    }
  })
  
  observeEvent(input$run_prop_test, {
    req(input$prop_test_type)
    
    result <- NULL
    if (input$prop_test_type == "1 Kelompok") {
      req(input$prop_1_x, input$prop_1_n, input$prop_1_p, input$prop_1_alt)
      if (input$prop_1_x > input$prop_1_n) {
        output$prop_test_output <- renderPrint("Error: Jumlah kejadian (x) tidak boleh lebih besar dari total observasi (n).")
        output$prop_test_interpret <- renderPrint("")
        return()
      }
      result <- prop.test(input$prop_1_x, input$prop_1_n, p = input$prop_1_p, alternative = input$prop_1_alt)
    } else {
      req(input$prop_2_x1, input$prop_2_n1, input$prop_2_x2, input$prop_2_n2)
      if (input$prop_2_x1 > input$prop_2_n1 || input$prop_2_x2 > input$prop_2_n2) {
        output$prop_test_output <- renderPrint("Error: Jumlah kejadian (x) tidak boleh lebih besar dari total observasi (n) di salah satu kelompok.")
        output$prop_test_interpret <- renderPrint("")
        return()
      }
      x_vec <- c(input$prop_2_x1, input$prop_2_x2)
      n_vec <- c(input$prop_2_n1, input$prop_2_n2)
      result <- prop.test(x_vec, n_vec)
    }
    
    output$prop_test_output <- renderPrint({
      print(result)
    })
    output$prop_test_interpret <- renderText({
      p_val <- result$p.value
      interpret <- ""
      if (input$prop_test_type == "1 Kelompok") {
        h0_text <- paste0("Proporsi populasi (p) sama dengan ", input$prop_1_p, ".")
        h1_text <- switch(input$prop_1_alt,
                          "two.sided" = paste0("Proporsi populasi (p) tidak sama dengan ", input$prop_1_p, "."),
                          "less" = paste0("Proporsi populasi (p) kurang dari ", input$prop_1_p, "."),
                          "greater" = paste0("Proporsi populasi (p) lebih dari ", input$prop_1_p, "."))
        
        interpret <- paste0(
          "Hipotesis Nol (H0): ", h0_text, "\n",
          "Hipotesis Alternatif (H1): ", h1_text, "\n",
          "Jenis Uji: Uji Proporsi 1 Kelompok.\n",
          "Nilai p (p-value): ", format(round(p_val, 4), scientific = FALSE), ".\n"
        )
      } else {
        h0_text <- "Proporsi kedua populasi adalah sama (p1 = p2)."
        h1_text <- "Proporsi kedua populasi tidak sama (p1 ≠ p2)."
        
        interpret <- paste0(
          "Hipotesis Nol (H0): ", h0_text, "\n",
          "Hipotesis Alternatif (H1): ", h1_text, "\n",
          "Jenis Uji: Uji Proporsi 2 Kelompok.\n",
          "Nilai p (p-value): ", format(round(p_val, 4), scientific = FALSE), ".\n"
        )
      }
      
      if (p_val < 0.05) {
        interpret <- paste0(interpret,
                            "Karena nilai p < 0.05, kita menolak H0.\n",
                            "Ini berarti ada bukti statistik yang signifikan bahwa ", h1_text, " pada tingkat signifikansi 5%.")
      } else {
        interpret <- paste0(interpret,
                            "Karena nilai p >= 0.05, kita gagal menolak H0.\n",
                            "Ini berarti tidak ada cukup bukti statistik untuk menyimpulkan bahwa ", h1_text, " pada tingkat signifikansi 5%.")
      }
      interpret
    })
  })
  
  observeEvent(input$run_var_test, {
    req(input$var_test_num_var, input$var_test_cat_var, input$var_test_alt)
    
    if (input$var_test_cat_var == "Pilih Kategori") {
      output$var_test_output <- renderPrint("Silakan pilih variabel kategorik (grup) untuk uji varians.")
      output$var_test_interpret <- renderPrint("")
      return()
    }
    
    plot_data <- left_join(sovi_data, stored_categorized_data(), by = "DISTRICTCODE")
    
    num_var <- plot_data[[input$var_test_num_var]]
    cat_var <- plot_data[[input$var_test_cat_var]]
    
    if (!is.factor(cat_var)) {
      cat_var <- as.factor(cat_var)
    }
    if (nlevels(cat_var) != 2) {
      output$var_test_output <- renderPrint("Error: Variabel kategorik harus memiliki tepat 2 level/kelompok untuk uji varians 2 kelompok.")
      output$var_test_interpret <- renderPrint("")
      return()
    }
    
    valid_data <- data.frame(num = num_var, cat = cat_var)
    valid_data <- na.omit(valid_data)
    
    if (nrow(valid_data) < 2 || any(table(valid_data$cat) < 2)) {
      output$var_test_output <- renderPrint("Error: Variabel tidak memiliki cukup data (minimal 2 observasi per grup setelah menghilangkan NA dan memfilter 2 kelompok).")
      output$var_test_interpret <- renderPrint("")
      return()
    }
    
    result <- var.test(valid_data$num ~ valid_data$cat, alternative = input$var_test_alt)
    output$var_test_output <- renderPrint({
      print(result)
    })
    output$var_test_interpret <- renderText({
      p_val <- result$p.value
      group_names <- levels(valid_data$cat)
      h0_text <- paste0("Varians populasi ", group_names[1], " sama dengan varians populasi ", group_names[2], ".")
      h1_text <- switch(input$var_test_alt,
                        "two.sided" = paste0("Varians populasi ", group_names[1], " tidak sama dengan varians populasi ", group_names[2], "."),
                        "less" = paste0("Varians populasi ", group_names[1], " kurang dari varians populasi ", group_names[2], "."),
                        "greater" = paste0("Varians populasi ", group_names[1], " lebih dari varians populasi ", group_names[2], "."))
      
      interpret <- paste0(
        "Hipotesis Nol (H0): ", h0_text, "\n",
        "Hipotesis Alternatif (H1): ", h1_text, "\n",
        "Uji yang Digunakan: F-test (var.test).\n",
        "Nilai p (p-value): ", format(round(p_val, 4), scientific = FALSE), ".\n"
      )
      
      if (p_val < 0.05) {
        interpret <- paste0(interpret,
                            "Karena nilai p < 0.05, kita menolak H0.\n",
                            "Ini berarti ada bukti statistik yang signifikan bahwa ", h1_text, " pada tingkat signifikansi 5%.")
      } else {
        interpret <- paste0(interpret,
                            "Karena nilai p >= 0.05, kita gagal menolak H0.\n",
                            "Ini berarti tidak ada cukup bukti statistik untuk menyimpulkan bahwa ", h1_text, " pada tingkat signifikansi 5%.")
      }
      interpret
    })
  })
  
  observeEvent(input$run_anova, {
    req(input$anova_type)
    
    anova_data <- left_join(sovi_data, stored_categorized_data(), by = "DISTRICTCODE")
    
    num_var_name <- ""
    if (input$anova_type == "One-Way ANOVA") {
      req(input$anova_1w_num_var, input$anova_1w_cat_var)
      num_var_name <- input$anova_1w_num_var
      cat_var_name <- input$anova_1w_cat_var
      
      if (cat_var_name == "Pilih Kategori") {
        output$anova_output <- renderPrint("Silakan pilih variabel kategorik untuk One-Way ANOVA.")
        output$anova_interpret <- renderPrint("")
        return()
      }
      
      cat_var <- anova_data[[cat_var_name]]
      if (!is.factor(cat_var)) {
        cat_var <- as.factor(cat_var)
      }
      if (nlevels(cat_var) < 2) {
        output$anova_output <- renderPrint("Error: Variabel kategorik harus memiliki setidaknya 2 level untuk ANOVA.")
        output$anova_interpret <- renderPrint("")
        return()
      }
      if (any(table(cat_var) < 1)) {
        output$anova_output <- renderPrint("Error: Setidaknya satu kelompok dalam variabel kategorik memiliki kurang dari 1 observasi.")
        output$anova_interpret <- renderPrint("")
        return()
      }
      
      formula_str <- paste0(num_var_name, " ~ ", cat_var_name)
    } else {
      req(input$anova_2w_num_var, input$anova_2w_cat_var1, input$anova_2w_cat_var2)
      num_var_name <- input$anova_2w_num_var
      cat_var1_name <- input$anova_2w_cat_var1
      cat_var2_name <- input$anova_2w_cat_var2
      
      if (cat_var1_name == "Pilih Kategori" || cat_var2_name == "Pilih Kategori") {
        output$anova_output <- renderPrint("Silakan pilih kedua variabel kategorik untuk Two-Way ANOVA.")
        output$anova_interpret <- renderPrint("")
        return()
      }
      
      cat_var1 <- anova_data[[cat_var1_name]]
      cat_var2 <- anova_data[[cat_var2_name]]
      
      if (!is.factor(cat_var1)) cat_var1 <- as.factor(cat_var1)
      if (!is.factor(cat_var2)) cat_var2 <- as.factor(cat_var2)
      
      if (nlevels(cat_var1) < 2 || nlevels(cat_var2) < 2) {
        output$anova_output <- renderPrint("Error: Kedua variabel kategorik harus memiliki setidaknya 2 level untuk Two-Way ANOVA.")
        output$anova_interpret <- renderPrint("")
        return()
      }
      if (any(table(cat_var1, cat_var2) < 1)) {
        output$anova_output <- renderPrint("Error: Setidaknya satu kombinasi kelompok memiliki kurang dari 1 observasi.")
        output$anova_interpret <- renderPrint("")
        return()
      }
      
      
      if (input$anova_2w_interaction) {
        formula_str <- paste0(num_var_name, " ~ ", cat_var1_name, " * ", cat_var2_name)
      } else {
        formula_str <- paste0(num_var_name, " ~ ", cat_var1_name, " + ", cat_var2_name)
      }
    }
    
    vars_for_anova <- c(num_var_name)
    if (input$anova_type == "One-Way ANOVA") {
      vars_for_anova <- c(vars_for_anova, input$anova_1w_cat_var)
    } else {
      vars_for_anova <- c(vars_for_anova, input$anova_2w_cat_var1, input$anova_2w_cat_var2)
    }
    
    clean_anova_data <- anova_data %>% select(all_of(vars_for_anova)) %>% na.omit()
    
    if (nrow(clean_anova_data) == 0) {
      output$anova_output <- renderPrint("Error: Tidak ada data valid untuk menjalankan ANOVA setelah menghilangkan missing values.")
      output$anova_interpret <- renderPrint("")
      return()
    }
    
    if (input$anova_type == "One-Way ANOVA") {
      clean_anova_data[[input$anova_1w_cat_var]] <- as.factor(clean_anova_data[[input$anova_1w_cat_var]])
    } else {
      clean_anova_data[[input$anova_2w_cat_var1]] <- as.factor(clean_anova_data[[input$anova_2w_cat_var1]])
      clean_anova_data[[input$anova_2w_cat_var2]] <- as.factor(clean_anova_data[[input$anova_2w_cat_var2]])
    }
    
    
    model <- aov(as.formula(formula_str), data = clean_anova_data)
    result_summary <- summary(model)
    
    output$anova_output <- renderPrint({
      print(result_summary)
    })
    output$anova_interpret <- renderText({
      p_vals <- result_summary[[1]]$`Pr(>F)`
      factor_names <- rownames(result_summary[[1]])
      
      interpret_lines <- character(0)
      
      if (input$anova_type == "One-Way ANOVA") {
        main_effect_p <- p_vals[factor_names == input$anova_1w_cat_var]
        if (!is.null(main_effect_p) && length(main_effect_p) > 0) {
          if (main_effect_p < 0.05) {
            interpret_lines <- c(interpret_lines,
                                 paste0("Karena nilai p untuk faktor '", input$anova_1w_cat_var, "' adalah ", format(round(main_effect_p, 4), scientific = FALSE), " (< 0.05), kita menolak H0.\n",
                                        "Ini berarti ada perbedaan rata-rata yang signifikan dari variabel ", num_var_name, " di antara setidaknya dua kelompok dalam faktor '", input$anova_1w_cat_var, "'.")
            )
            interpret_lines <- c(interpret_lines, "Untuk mengetahui kelompok mana yang berbeda, diperlukan uji post-hoc (misalnya Tukey HSD).")
          } else {
            interpret_lines <- c(interpret_lines,
                                 paste0("Karena nilai p untuk faktor '", input$anova_1w_cat_var, "' adalah ", format(round(main_effect_p, 4), scientific = FALSE), " (>= 0.05), kita gagal menolak H0.\n",
                                        "Ini berarti tidak ada perbedaan rata-rata yang signifikan dari variabel ", num_var_name, " di antara kelompok-kelompok dalam faktor '", input$anova_1w_cat_var, "'.")
            )
          }
        }
      } else {
        interpret_lines <- c(interpret_lines, "Analisis untuk Two-Way ANOVA:")
        if (input$anova_2w_interaction) {
          interaction_name <- paste0(cat_var1_name, ":", cat_var2_name)
          interaction_p <- p_vals[grepl(interaction_name, factor_names)]
          if (!is.null(interaction_p) && length(interaction_p) > 0) {
            if (interaction_p < 0.05) {
              interpret_lines <- c(interpret_lines,
                                   paste0("Interaksi (", interaction_name, "): p-value = ", format(round(interaction_p, 4), scientific = FALSE), " (< 0.05). Ada efek interaksi yang signifikan.\n",
                                          "Ini berarti efek satu faktor terhadap rata-rata variabel dependen bergantung pada level faktor yang lain. Fokus interpretasi pada interaksi.")
              )
            } else {
              interpret_lines <- c(interpret_lines,
                                   paste0("Interaksi (", interaction_name, "): p-value = ", format(round(interaction_p, 4), scientific = FALSE), " (>= 0.05). Tidak ada efek interaksi yang signifikan.\n",
                                          "Ini berarti efek satu faktor terhadap rata-rata variabel dependen tidak bergantung pada level faktor yang lain. Fokus interpretasi pada efek utama.")
              )
            }
          }
        }
        
        main_effect1_p <- p_vals[factor_names == cat_var1_name]
        if (!is.null(main_effect1_p) && length(main_effect1_p) > 0) {
          if (main_effect1_p < 0.05) {
            interpret_lines <- c(interpret_lines,
                                 paste0("Efek Utama '", cat_var1_name, "': p-value = ", format(round(main_effect1_p, 4), scientific = FALSE), " (< 0.05). Efek signifikan.\n",
                                        "Ada perbedaan rata-rata yang signifikan dari variabel ", num_var_name, " di antara level faktor '", cat_var1_name, "'.")
            )
          } else {
            interpret_lines <- c(interpret_lines,
                                 paste0("Efek Utama '", cat_var1_name, "': p-value = ", format(round(main_effect1_p, 4), scientific = FALSE), " (>= 0.05). Efek tidak signifikan.")
            )
          }
        }
        
        main_effect2_p <- p_vals[factor_names == cat_var2_name]
        if (!is.null(main_effect2_p) && length(main_effect2_p) > 0) {
          if (main_effect2_p < 0.05) {
            interpret_lines <- c(interpret_lines,
                                 paste0("Efek Utama '", cat_var2_name, "': p-value = ", format(round(main_effect2_p, 4), scientific = FALSE), " (< 0.05). Efek signifikan.\n",
                                        "Ada perbedaan rata-rata yang signifikan dari variabel ", num_var_name, " di antara level faktor '", cat_var2_name, "'.")
            )
          } else {
            interpret_lines <- c(interpret_lines,
                                 paste0("Efek Utama '", cat_var2_name, "': p-value = ", format(round(main_effect2_p, 4), scientific = FALSE), " (>= 0.05). Efek tidak signifikan.")
            )
          }
        }
      }
      paste(interpret_lines, collapse = "\n")
    })
  })
  
  # --- Regresi Linear Berganda Server ---
  # UI Dinamis untuk variabel independen
  output$reg_independent_vars_ui <- renderUI({
    req(input$reg_dependent_var)
    
    available_numeric_ivs <- numeric_cols[numeric_cols != input$reg_dependent_var]
    
    available_categorical_ivs <- all_categorical_vars_reactive()
    if (length(available_categorical_ivs) == 1 && available_categorical_ivs[1] == "Tidak Ada Variabel Kategori Tersedia") {
      available_categorical_ivs <- c()
    }
    
    all_potential_ivs <- c(available_numeric_ivs, available_categorical_ivs)
    
    checkboxGroupInput("reg_independent_vars", "Pilih Variabel Independen:",
                       choices = all_potential_ivs,
                       selected = NULL)
  })
  
  
  # Reactive untuk model regresi
  linear_model <- eventReactive(input$run_regression, {
    req(input$reg_dependent_var)
    if (is.null(input$reg_independent_vars) || length(input$reg_independent_vars) == 0) {
      showNotification("Pilih setidaknya satu variabel independen.", type = "error")
      return(NULL)
    }
    
    reg_data_full <- left_join(sovi_data, stored_categorized_data(), by = "DISTRICTCODE")
    
    model_vars_names <- c(input$reg_dependent_var, input$reg_independent_vars)
    
    clean_reg_data <- reg_data_full %>% select(all_of(model_vars_names))
    
    for (iv in input$reg_independent_vars) {
      if (iv %in% all_categorical_vars_reactive() && !is.factor(clean_reg_data[[iv]])) {
        clean_reg_data[[iv]] <- as.factor(clean_reg_data[[iv]])
      }
    }
    
    clean_reg_data <- na.omit(clean_reg_data)
    
    if (nrow(clean_reg_data) == 0) {
      showNotification("Tidak ada data valid untuk menjalankan regresi setelah menghilangkan missing values.", type = "error")
      return(NULL)
    }
    
    formula_str <- paste0(input$reg_dependent_var, " ~ ", paste(input$reg_independent_vars, collapse = " + "))
    
    lm(as.formula(formula_str), data = clean_reg_data)
  })
  
  # Output Ringkasan Model
  output$regression_summary <- renderPrint({
    model <- linear_model()
    if (!is.null(model)) {
      summary(model)
    } else {
      cat("Model belum dijalankan atau ada error.")
    }
  })
  
  # Interpretasi Model Regresi
  output$regression_interpret <- renderText({
    model_summary <- summary(linear_model())
    if (is.null(model_summary)) return("")
    
    interpret_lines <- character(0)
    
    f_stat <- model_summary$fstatistic[1]
    f_p_val <- pf(f_stat, model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    
    interpret_lines <- c(interpret_lines,
                         paste0("Model ini menjelaskan sekitar ", round(r_squared * 100, 2), "% varians dari variabel dependen (R-squared). R-squared yang disesuaikan adalah ", round(adj_r_squared * 100, 2), "%."),
                         paste0("Uji F model secara keseluruhan memiliki p-value = ", format(round(f_p_val, 4), scientific = FALSE), "."))
    if (f_p_val < 0.05) {
      interpret_lines <- c(interpret_lines, "Karena p-value F-test < 0.05, model regresi secara keseluruhan signifikan secara statistik. Ini berarti setidaknya satu variabel independen memiliki hubungan linear yang signifikan dengan variabel dependen.")
    } else {
      interpret_lines <- c(interpret_lines, "Karena p-value F-test >= 0.05, model regresi secara keseluruhan tidak signifikan secara statistik.")
    }
    
    interpret_lines <- c(interpret_lines, "\nInterpretasi Koefisien Variabel Independen:")
    coefs <- as.data.frame(model_summary$coefficients)
    for (i in 2:nrow(coefs)) {
      var_name <- rownames(coefs)[i]
      estimate <- round(coefs[i, "Estimate"], 4)
      p_val_coef <- coefs[i, "Pr(>|t|)"]
      
      if (p_val_coef < 0.05) {
        interpret_lines <- c(interpret_lines,
                             paste0("Koefisien untuk '", var_name, "' adalah ", estimate, " (p-value = ", format(round(p_val_coef, 4), scientific = FALSE), "). Ini signifikan secara statistik.\n",
                                    "   Artinya, setiap peningkatan satu unit pada ", var_name, " diasosiasikan dengan perubahan rata-rata ", estimate, " unit pada ", input$reg_dependent_var, ", dengan mengontrol variabel lain.")
        )
      } else {
        interpret_lines <- c(interpret_lines,
                             paste0("Koefisien untuk '", var_name, "' adalah ", estimate, " (p-value = ", format(round(p_val_coef, 4), scientific = FALSE), "). Ini tidak signifikan secara statistik.\n",
                                    "   Artinya, tidak ada bukti statistik yang cukup untuk menyimpulkan bahwa ", var_name, " memiliki hubungan linear yang signifikan dengan ", input$reg_dependent_var, ", dengan mengontrol variabel lain.")
        )
      }
    }
    paste(interpret_lines, collapse = "\n")
  })
  
  # Asumsi 1: Linearitas (Plot Residuals vs Fitted)
  output$residuals_fitted_plot <- renderPlot({
    model <- linear_model()
    if (!is.null(model)) {
      plot(fitted(model), residuals(model),
           xlab = "Nilai Prediksi (Fitted Values)",
           ylab = "Residuals",
           main = "Plot Residuals vs Fitted Values")
      abline(h = 0, col = "red", lty = 2)
      lines(lowess(fitted(model), residuals(model)), col = "blue", lty = 1)
    } else {
      plot(NULL, xlim=0:1, ylim=0:1, main="Plot Residuals vs Fitted Values", xlab="", ylab="", type="n")
      text(0.5, 0.5, "Model belum dijalankan untuk plot ini.")
    }
  })
  
  output$linearity_interpret <- renderText({
    model <- linear_model()
    if (is.null(model)) return("Jalankan model regresi terlebih dahulu untuk melihat interpretasi linearitas.")
    "Interpretasi: Plot ini menunjukkan residual terhadap nilai prediksi. Jika asumsi linearitas terpenuhi, titik-titik harus tersebar secara acak di sekitar garis horizontal di nol tanpa pola yang jelas (misalnya, bentuk U, kerucut, atau kurva). Garis biru (lowess) harus mendekati garis merah (y=0). Pola yang jelas mengindikasikan pelanggaran asumsi linearitas."
  })
  
  # Asumsi 2: Normalitas Residual
  output$normality_residuals_output <- renderPrint({
    model <- linear_model()
    if (!is.null(model)) {
      res <- residuals(model)
      if (length(unique(res)) < 5) {
        cat("Error: Residual tidak memiliki cukup nilai unik untuk uji normalitas.")
        return(NULL)
      }
      ad.test(res)
    } else {
      cat("Model belum dijalankan untuk uji normalitas residual.")
    }
  })
  
  output$normality_residuals_interpret <- renderText({
    model <- linear_model()
    if (is.null(model)) return("Jalankan model regresi terlebih dahulu untuk melihat interpretasi normalitas residual.")
    
    res <- residuals(model)
    if (length(unique(res)) < 5) {
      return("Tidak cukup nilai unik pada residual untuk melakukan uji normalitas.")
    }
    
    norm_test_result <- ad.test(res)
    p_val <- norm_test_result$p.value
    
    interpret <- paste0("Hasil Uji Normalitas Residual (Anderson-Darling Test):\nNilai p (p-value): ", format(round(p_val, 4), scientific = FALSE), ".\n")
    if (p_val < 0.05) {
      interpret <- paste0(interpret, "Karena nilai p < 0.05, kita menolak H0.\nIni berarti ada bukti statistik yang kuat bahwa residual TIDAK berdistribusi normal.\nPelanggaran asumsi normalitas bisa mengindikasikan bahwa model mungkin tidak optimal atau ada outlier.")
    } else {
      interpret <- paste0(interpret, "Karena nilai p >= 0.05, kita gagal menolak H0.\nIni berarti tidak ada cukup bukti statistik untuk menyimpulkan bahwa residual TIDAK berdistribusi normal (sering diinterpretasikan sebagai 'residual berdistribusi normal').")
    }
    interpret
  })
  
  # Asumsi 3: Homoskedastisitas
  output$homoscedasticity_output <- renderPrint({
    model <- linear_model()
    if (!is.null(model)) {
      ncvTest(model)
    } else {
      cat("Model belum dijalankan untuk uji homoskedastisitas.")
    }
  })
  
  output$homoscedasticity_interpret <- renderText({
    model <- linear_model()
    if (is.null(model)) return("Jalankan model regresi terlebih dahulu untuk melihat interpretasi homoskedastisitas.")
    
    ncv_test_result <- ncvTest(model)
    p_val <- ncv_test_result$p
    
    interpret <- paste0("Hasil Uji Homoskedastisitas (Non-Constant Variance Test):\nNilai p (p-value): ", format(round(p_val, 4), scientific = FALSE), ".\n")
    if (p_val < 0.05) {
      interpret <- paste0(interpret, "Karena nilai p < 0.05, kita menolak H0.\nIni berarti ada bukti statistik yang kuat bahwa varians residual TIDAK konstan (Heteroskedastisitas).\nPelanggaran asumsi ini dapat menyebabkan estimasi standar error yang bias dan kesimpulan inferensial yang tidak akurat.")
    } else {
      interpret <- paste0(interpret, "Karena nilai p >= 0.05, kita gagal menolak H0.\nIni berarti tidak ada cukup bukti statistik untuk menyimpulkan bahwa varians residual TIDAK konstan (sering diinterpretasikan sebagai 'varians residual adalah konstan / Homoskedastisitas').")
    }
    interpret
  })
  
  # Asumsi 4: Independensi Residual
  output$durbin_watson_output <- renderPrint({
    model <- linear_model()
    if (!is.null(model)) {
      durbinWatsonTest(model)
    } else {
      cat("Model belum dijalankan untuk uji Durbin-Watson.")
    }
  })
  
  output$durbin_watson_interpret <- renderText({
    model <- linear_model()
    if (is.null(model)) return("Jalankan model regresi terlebih dahulu untuk melihat interpretasi independensi residual.")
    
    dw_test_result <- durbinWatsonTest(model)
    dw_value <- dw_test_result$dw[1]
    dw_p_val <- dw_test_result$p[1]
    
    interpret <- paste0("Hasil Uji Independensi Residual (Durbin-Watson Test):\nNilai Durbin-Watson (d): ", round(dw_value, 4), "\nNilai p (p-value): ", format(round(dw_p_val, 4), scientific = FALSE), ".\n")
    if (dw_p_val < 0.05) {
      interpret <- paste0(interpret, "Karena nilai p < 0.05, kita menolak H0.\nIni berarti ada bukti statistik yang signifikan bahwa residual TIDAK independen (ada autokorelasi).\nAutokorelasi dapat terjadi pada data deret waktu atau data spasial dan dapat menyebabkan standar error yang bias.")
    } else {
      interpret <- paste0(interpret, "Karena nilai p >= 0.05, kita gagal menolak H0.\nIni berarti tidak ada cukup bukti statistik untuk menyimpulkan bahwa residual TIDAK independen (sering diinterpretasikan sebagai 'residual adalah independen').\nNilai d mendekati 2 juga mendukung asumsi independensi.")
    }
    interpret
  })
  
  # Asumsi 5: Tidak Ada Multikolinearitas
  output$vif_output <- renderPrint({
    model <- linear_model()
    if (!is.null(model) && length(input$reg_independent_vars) > 1) {
      vif(model)
    } else if (is.null(model)) {
      cat("Model belum dijalankan untuk uji multikolinearitas.")
    } else {
      cat("VIF hanya relevan jika ada dua atau lebih variabel independen.")
    }
  })
  
  output$vif_interpret <- renderText({
    model <- linear_model()
    if (is.null(model)) return("Jalankan model regresi terlebih dahulu untuk melihat interpretasi multikolinearitas.")
    if (length(input$reg_independent_vars) <= 1) return("VIF hanya relevan jika ada dua atau lebih variabel independen. Tidak ada multikolinearitas yang perlu diuji dengan hanya satu prediktor.")
    
    vif_results <- vif(model)
    interpret_lines <- character(0)
    
    has_high_vif <- FALSE
    for (i in seq_along(vif_results)) {
      var_name <- names(vif_results)[i]
      vif_value <- vif_results[i]
      if (vif_value >= 5) { # Umumnya VIF > 5 atau > 10 dianggap tinggi
        has_high_vif <- TRUE
        interpret_lines <- c(interpret_lines,
                             paste0("Variabel '", var_name, "' memiliki VIF = ", round(vif_value, 2), " (tinggi)."))
      } else {
        interpret_lines <- c(interpret_lines,
                             paste0("Variabel '", var_name, "' memiliki VIF = ", round(vif_value, 2), " (aman)."))
      }
    }
    
    if (has_high_vif) {
      interpret_lines <- c(interpret_lines, "\nMultikolinearitas dapat membuat koefisien regresi tidak stabil dan sulit diinterpretasikan. Pertimbangkan untuk menghapus salah satu variabel yang sangat berkorelasi atau menggunakan teknik lain (misalnya PCA, regresi ridge).")
    } else {
      interpret_lines <- c(interpret_lines, "\nNilai VIF (Variance Inflation Factor) menunjukkan tidak ada multikolinearitas yang signifikan antar variabel independen yang dipilih (semua VIF < 5 atau < 10).")
    }
    
    paste(interpret_lines, collapse = "\n")
  })
  
  # Download Handler untuk Dataset Asli (sovi_data.csv)
  output$download_sovi_csv <- downloadHandler(
    filename = function() {
      # Nama file yang akan diunduh, tambahkan tanggal untuk keunikan
      paste("sovi_data",".csv", sep = "")
    },
    content = function(file) {
      # Pastikan 'sovi_data' dapat diakses di sini.
      # Karena 'sovi_data' dimuat secara global di awal app.R,
      # seharusnya bisa langsung diakses.
      write.csv(sovi_data, file, row.names = FALSE)
    }
  )
  
  # NEW: Download Handlers for Eksplorasi Data Plots
  output$download_histogram_plot <- downloadHandler(
    filename = function() {
      paste("histogram_", input$hist_var, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(input$hist_var)
      plot_obj <- ggplot(sovi_data, aes_string(x = input$hist_var)) +
        geom_histogram(binwidth = diff(range(sovi_data[[input$hist_var]], na.rm = TRUE))/30, fill = "steelblue", color = "black") +
        labs(title = paste("Histogram", input$hist_var), x = input$hist_var, y = "Frekuensi") +
        theme_minimal()
      ggsave(file, plot = plot_obj, device = "png", width = 8, height = 6, units = "in", dpi = 300)
    }
  )
  
  output$download_boxplot_plot <- downloadHandler(
    filename = function() {
      if (input$boxplot_cat_var == "None") {
        paste("boxplot_", input$boxplot_num_var, "_", Sys.Date(), ".png", sep = "")
      } else {
        paste("boxplot_", input$boxplot_num_var, "_by_", input$boxplot_cat_var, "_", Sys.Date(), ".png", sep = "")
      }
    },
    content = function(file) {
      req(input$boxplot_num_var)
      plot_obj <- NULL
      if (input$boxplot_cat_var == "None") {
        plot_obj <- ggplot(sovi_data, aes_string(y = input$boxplot_num_var)) +
          geom_boxplot(fill = "lightblue") +
          labs(title = paste("Box Plot", input$boxplot_num_var), y = input$boxplot_num_var) +
          theme_minimal() +
          coord_flip()
      } else {
        plot_data <- left_join(sovi_data, stored_categorized_data(), by = "DISTRICTCODE")
        
        if (!(input$boxplot_cat_var %in% names(plot_data))) {
          warning("Variabel kategorik yang dipilih tidak ditemukan setelah penggabungan data saat download boxplot.")
          plot_obj <- ggplot() + labs(title = "Variabel kategori tidak ditemukan.") + theme_void()
        } else {
          num_var <- plot_data[[input$boxplot_num_var]]
          cat_var <- plot_data[[input$boxplot_cat_var]]
          valid_indices <- !is.na(num_var) & !is.na(cat_var)
          plot_data_filtered <- plot_data[valid_indices, ]
          
          if (nrow(plot_data_filtered) == 0) {
            plot_obj <- ggplot() + labs(title = "Tidak ada data yang valid untuk plot boxplot setelah pembersihan NA.") + theme_void()
          } else {
            plot_data_filtered[[input$boxplot_cat_var]] <- as.factor(plot_data_filtered[[input$boxplot_cat_var]])
            
            if (nlevels(plot_data_filtered[[input$boxplot_cat_var]]) < 2) {
              plot_obj <- ggplot(plot_data_filtered, aes_string(y = input$boxplot_num_var)) +
                geom_boxplot(fill = "lightblue") +
                labs(title = paste("Box Plot", input$boxplot_num_var, "(Variabel Kategorik hanya memiliki 1 level/data tidak cukup)"), y = input$boxplot_num_var) +
                theme_minimal() +
                coord_flip()
            } else {
              plot_obj <- ggplot(plot_data_filtered, aes_string(x = input$boxplot_cat_var, y = input$boxplot_num_var, fill = input$boxplot_cat_var)) +
                geom_boxplot() +
                labs(title = paste("Box Plot", input$boxplot_num_var, "berdasarkan", input$boxplot_cat_var),
                     x = input$boxplot_cat_var, y = input$boxplot_num_var) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            }
          }
        }
      }
      if (!is.null(plot_obj)) {
        ggsave(file, plot = plot_obj, device = "png", width = 8, height = 6, units = "in", dpi = 300)
      } else {
        cat("Error: Could not generate box plot for download.", file = file)
      }
    }
  )
  
  output$download_scatterplot_plot <- downloadHandler(
    filename = function() {
      paste("scatterplot_", input$scatter_x_var, "_vs_", input$scatter_y_var, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(input$scatter_x_var, input$scatter_y_var)
      plot_obj <- ggplot(sovi_data, aes_string(x = input$scatter_x_var, y = input$scatter_y_var)) +
        geom_point(alpha = 0.6) +
        labs(title = paste("Scatter Plot", input$scatter_x_var, "vs", input$scatter_y_var),
             x = input$scatter_x_var, y = input$scatter_y_var) +
        theme_minimal()
      ggsave(file, plot = plot_obj, device = "png", width = 8, height = 6, units = "in", dpi = 300)
    }
  )

  # --- Download Handler untuk Eksplorasi Data (RMD Output) ---
  output$download_eksplorasi_report <- downloadHandler(
    filename = function() {
      paste("laporan_eksplorasi_data_", Sys.Date(), ".Rmd", sep = "")
    },
    content = function(file) {
      # Construct the RMarkdown content as a string
      rmd_content <- paste0(
        "---", "\n",
        "title: \"Laporan Eksplorasi Data\"", "\n",
        "author: \"Shiny App Analisis Data SoVI\"", "\n",
        "date: \"", Sys.Date(), "\"", "\n",
        "output: html_document", "\n",
        "---", "\n\n",
        "```{r setup, include=FALSE}", "\n",
        "knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 6)", "\n",
        "library(dplyr)", "\n",
        "library(ggplot2)", "\n",
        "library(rmarkdown)", "\n",
        "# PASTIKAN FILE 'sovi_data.csv' DAN 'categorized_data.csv' (jika ada) BERADA DI DIREKTORI YANG SAMA DENGAN FILE RMD INI.", "\n",
        "sovi_data <- read.csv('sovi_data.csv')", "\n",
        "stored_categorized_data <- data.frame(DISTRICTCODE = character(0)) # Initialize empty", "\n",
        "tryCatch({", "\n",
        "  if (file.exists('categorized_data.csv')) {", "\n",
        "    temp_cat_data <- read.csv('categorized_data.csv')", "\n",
        "    if (ncol(temp_cat_data) > 1) { # Check if it contains actual categorized columns", "\n",
        "      stored_categorized_data <- temp_cat_data", "\n",
        "    }", "\n",
        "  }", "\n",
        "}, error = function(e) {", "\n",
        "  message('File categorized_data.csv tidak ditemukan atau tidak dapat dimuat: ', e$message)", "\n",
        "})", "\n",
        "plot_data_full <- left_join(sovi_data, stored_categorized_data, by = 'DISTRICTCODE')", "\n",
        "```", "\n\n",
        
        "## Eksplorasi Data", "\n\n",
        
        "### Statistik Deskriptif", "\n",
        "Variabel Numerik yang Dipilih: `", input$desc_stats_var, "`\n\n",
        "```{r descriptive-stats-output}", "\n",
        "summary(sovi_data[['", input$desc_stats_var, "']])", "\n",
        "```", "\n",
        "**Interpretasi Statistik Deskriptif:** Bagian ini menampilkan ringkasan statistik (rata-rata, median, standar deviasi, min, maks, kuartil) untuk variabel numerik yang Anda pilih. Ini membantu memahami distribusi dan karakteristik dasar data seperti pusat, sebaran, dan keberadaan outlier.", "\n\n",
        
        "### Histogram", "\n",
        "Variabel Numerik yang Dipilih: `", input$hist_var, "`\n\n",
        "```{r histogram-plot}", "\n",
        "ggplot(sovi_data, aes_string(x = '", input$hist_var, "')) +", "\n",
        "  geom_histogram(binwidth = diff(range(sovi_data[['", input$hist_var, "']], na.rm = TRUE))/30, fill = 'steelblue', color = 'black') +", "\n",
        "  labs(title = paste('Histogram', '", input$hist_var, "'), x = '", input$hist_var, "', y = 'Frekuensi') +", "\n",
        "  theme_minimal()", "\n",
        "```", "\n",
        "**Interpretasi Histogram:** Histogram menunjukkan distribusi frekuensi data. Bentuk histogram dapat mengindikasikan apakah data cenderung berdistribusi normal, miring ke kiri (left-skewed), miring ke kanan (right-skewed), atau memiliki lebih dari satu puncak (bimodal/multimodal).", "\n\n",
        
        "### Box Plot", "\n",
        "Variabel Numerik (Y-axis): `", input$boxplot_num_var, "`\n",
        "Variabel Kategorik (X-axis): `", input$boxplot_cat_var, "`\n\n",
        "```{r boxplot-plot}", "\n",
        "if ('", input$boxplot_cat_var, "' == 'None') {", "\n",
        "  ggplot(sovi_data, aes_string(y = '", input$boxplot_num_var, "')) +", "\n",
        "    geom_boxplot(fill = 'lightblue') +", "\n",
        "    labs(title = paste('Box Plot', '", input$boxplot_num_var, "'), y = '", input$boxplot_num_var, "') +", "\n",
        "    theme_minimal() +", "\n",
        "    coord_flip()", "\n",
        "} else {", "\n",
        "  num_var_boxplot <- plot_data_full[['", input$boxplot_num_var, "']]", "\n",
        "  cat_var_boxplot <- plot_data_full[['", input$boxplot_cat_var, "']]", "\n",
        "  valid_indices_boxplot <- !is.na(num_var_boxplot) & !is.na(cat_var_boxplot)", "\n",
        "  plot_data_filtered_boxplot <- plot_data_full[valid_indices_boxplot, ]", "\n",
        "  plot_data_filtered_boxplot[['", input$boxplot_cat_var, "']] <- as.factor(plot_data_filtered_boxplot[['", input$boxplot_cat_var, "']])", "\n",
        "  if (nrow(plot_data_filtered_boxplot) > 0 && nlevels(plot_data_filtered_boxplot[['", input$boxplot_cat_var, "']]) >= 2) {", "\n",
        "    ggplot(plot_data_filtered_boxplot, aes_string(x = '", input$boxplot_cat_var, "', y = '", input$boxplot_num_var, "', fill = '", input$boxplot_cat_var, "')) +", "\n",
        "      geom_boxplot() +", "\n",
        "      labs(title = paste('Box Plot', '", input$boxplot_num_var, "', 'berdasarkan', '", input$boxplot_cat_var, "'),", "\n",
        "           x = '", input$boxplot_cat_var, "', y = '", input$boxplot_num_var, "') +", "\n",
        "      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +", "\n",
        "      theme_minimal()", "\n",
        "  } else {", "\n",
        "    ggplot(sovi_data, aes_string(y = '", input$boxplot_num_var, "')) +", "\n",
        "      geom_boxplot(fill = 'lightblue') +", "\n",
        "      labs(title = paste('Box Plot', '", input$boxplot_num_var, "', '(Variabel Kategorik tidak valid/data tidak cukup)'), y = '", input$boxplot_num_var, "') +", "\n",
        "      theme_minimal() +", "\n",
        "      coord_flip()", "\n",
        "  }", "\n",
        "}", "\n",
        "```", "\n",
        "**Interpretasi Box Plot:** Box plot menggambarkan median (garis tengah), kuartil (kotak), dan rentang data (whiskers). Titik di luar whiskers adalah outlier. Jika variabel kategorik dipilih, box plot akan menunjukkan perbandingan distribusi variabel numerik antar kategori.", "\n\n",
        
        "### Scatter Plot", "\n",
        "Variabel Numerik (X-axis): `", input$scatter_x_var, "`\n",
        "Variabel Numerik (Y-axis): `", input$scatter_y_var, "`\n\n",
        "```{r scatterplot-plot}", "\n",
        "ggplot(sovi_data, aes_string(x = '", input$scatter_x_var, "', y = '", input$scatter_y_var, "')) +", "\n",
        "  geom_point(alpha = 0.6) +", "\n",
        "  labs(title = paste('Scatter Plot', '", input$scatter_x_var, "', 'vs', '", input$scatter_y_var, "'),", "\n",
        "       x = '", input$scatter_x_var, "', y = '", input$scatter_y_var, "') +", "\n",
        "  theme_minimal()", "\n",
        "```", "\n",
        "**Interpretasi Scatter Plot:** Scatter plot menunjukkan hubungan atau korelasi antara dua variabel numerik. Pola titik-titik dapat mengindikasikan korelasi positif (naik), negatif (turun), atau tidak ada korelasi (menyebar acak).", "\n\n"
      )
      writeChar(rmd_content, file, eos = NULL)
    }
  )
  
  # --- Download Handler untuk Laporan Uji Asumsi (RMD Output) ---
  output$download_asumsi_report <- downloadHandler(
    filename = function() {
      paste("laporan_uji_asumsi_", Sys.Date(), ".Rmd", sep = "")
    },
    content = function(file) {
      # --- Persiapan Data untuk RMD ---
      has_categorized_data <- ncol(stored_categorized_data()) > 1
      if (has_categorized_data) {
        write.csv(stored_categorized_data(), "categorized_data.csv", row.names = FALSE)
      } else {
        if (file.exists("categorized_data.csv")) {
          file.remove("categorized_data.csv")
        }
        showNotification(
          "Peringatan: Tidak ada variabel kategori yang disimpan. Laporan .Rmd mungkin tidak mereplikasi hasil uji homogenitas yang menggunakan kategori kustom. Untuk menyertakan data kategori, silakan proses data di 'Manajemen Data' -> 'Transformasi Data' dan klik 'Simpan Data Kategori' terlebih dahulu.",
          type = "warning",
          duration = 8
        )
      }
      
      # --- Buat konten RMarkdown secara dinamis ---
      rmd_content <- paste0(
        "---", "\n",
        "title: \"Laporan Uji Asumsi Data\"", "\n",
        "author: \"Shiny App Analisis Data SoVI\"", "\n",
        "date: \"", Sys.Date(), "\"", "\n",
        "output: html_document", "\n",
        "---", "\n\n",
        
        "```{r setup, include=FALSE}", "\n",
        "knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)", "\n",
        "library(dplyr)", "\n",
        "library(nortest) # Untuk ad.test", "\n",
        "library(car) # Untuk leveneTest", "\n",
        "# PASTIKAN FILE 'sovi_data.csv' DAN 'categorized_data.csv' (jika ada) BERADA DI DIREKTORI YANG SAMA DENGAN FILE RMD INI.", "\n",
        "sovi_data <- read.csv('sovi_data.csv')", "\n",
        "stored_categorized_data <- data.frame(DISTRICTCODE = character(0))", "\n",
        "tryCatch({", "\n",
        "  if (file.exists('categorized_data.csv')) {", "\n",
        "    temp_cat_data <- read.csv('categorized_data.csv')", "\n",
        "    if (ncol(temp_cat_data) > 1 && all(sapply(temp_cat_data, class) %in% c('numeric', 'integer', 'factor', 'character'))) {", "\n",
        "      stored_categorized_data <- temp_cat_data", "\n",
        "    }", "\n",
        "  }", "\n",
        "}, error = function(e) {", "\n",
        "  message('File categorized_data.csv tidak ditemukan atau tidak dapat dimuat: ', e$message)", "\n",
        "})", "\n",
        "plot_data_full <- left_join(sovi_data, stored_categorized_data, by = 'DISTRICTCODE')", "\n",
        "```", "\n\n",
        
        "## Uji Asumsi Data", "\n\n",
        
        "### Uji Normalitas (Anderson-Darling Test)", "\n",
        "Uji normalitas digunakan untuk menentukan apakah sampel data berasal dari populasi yang berdistribusi normal.\n",
        "Hipotesis Nol (H0): Data berdistribusi normal.\n",
        "Hipotesis Alternatif (H1): Data tidak berdistribusi normal.\n",
        "Uji yang Digunakan: Anderson-Darling Test (ad.test dari paket `nortest`)\n",
        "Aturan Keputusan: Tolak H0 jika nilai p < tingkat signifikansi (misalnya 0.05).\n\n",
        "Variabel Numerik yang Dipilih: `", input$normality_var, "`\n\n",
        "```{r normality-test-output}", "\n",
        "selected_var_data <- sovi_data[['", input$normality_var, "']]", "\n",
        "selected_var_data <- na.omit(selected_var_data)", "\n",
        "if (length(unique(selected_var_data)) < 5) {", "\n",
        "  cat('Error: Variabel tidak memiliki cukup nilai unik (minimal 5) untuk uji normalitas.')", "\n",
        "} else {", "\n",
        "  norm_test_result <- ad.test(selected_var_data)", "\n",
        "  print(norm_test_result)", "\n",
        "}", "\n",
        "```", "\n",
        "**Interpretasi Hasil Uji Normalitas:**\n",
        "```{r normality-interpretation-text, echo=FALSE}", "\n",
        "selected_var_data <- sovi_data[['", input$normality_var, "']]", "\n",
        "selected_var_data <- na.omit(selected_var_data)", "\n",
        "if (length(unique(selected_var_data)) < 5) {", "\n",
        "  cat('Tidak cukup nilai unik pada variabel untuk melakukan uji normalitas.')", "\n",
        "} else {", "\n",
        "  norm_test_result <- ad.test(selected_var_data)", "\n",
        "  p_val <- norm_test_result$p.value", "\n",
        "  cat('Nilai p (', format(round(p_val, 4), scientific = FALSE), ')', if(p_val < 0.05) '<' else '>=', ' 0.05.\\n')", "\n",
        "  if (p_val < 0.05) {", "\n",
        "    cat('Karena nilai p < tingkat signifikansi, kita menolak Hipotesis Nol (H0).\\n')", "\n",
        "    cat('Ini berarti ada bukti statistik yang kuat bahwa data ', '", input$normality_var, "', ' TIDAK berdistribusi normal.\\n')", "\n",
        "  } else {", "\n",
        "    cat('Karena nilai p >= tingkat signifikansi, kita GAGAL menolak Hipotesis Nol (H0).\\n')", "\n",
        "    cat('Ini berarti tidak ada cukup bukti statistik untuk menyimpulkan bahwa data ', '", input$normality_var, "', ' berdistribusi normal.\\n')", "\n",
        "  }", "\n",
        "}", "\n",
        "```\n\n",
        
        "### Uji Homogenitas Varians (Levene's Test)", "\n",
        "Uji homogenitas varians digunakan untuk menentukan apakah varians antar kelompok yang berbeda adalah sama. Ini sering menjadi asumsi untuk uji seperti ANOVA.\n",
        "Hipotesis Nol (H0): Varians antar kelompok adalah homogen (sama).\n",
        "Hipotesis Alternatif (H1): Varians antar kelompok tidak homogen (berbeda).\n",
        "Uji yang Digunakan: Levene's Test (leveneTest dari paket `car`)\n",
        "Aturan Keputusan: Tolak H0 jika nilai p < tingkat signifikansi (misalnya 0.05).\n\n",
        "Variabel Numerik yang Dipilih: `", input$homogeneity_num_var, "`\n",
        "Variabel Kategorik yang Dipilih: `", input$homogeneity_cat_var, "`\n\n",
        "```{r homogeneity-test-output}", "\n",
        "selected_num_data <- plot_data_full[['", input$homogeneity_num_var, "']]", "\n",
        "if ('", input$homogeneity_cat_var, "' == 'Pilih Kategori') {", "\n",
        "  cat('Silakan pilih variabel kategorik (grup) untuk menjalankan Uji Homogenitas Varians.')", "\n",
        "} else if (!('", input$homogeneity_cat_var, "' %in% names(plot_data_full))) {", "\n",
        "  cat('Error: Variabel kategorik ', '", input$homogeneity_cat_var, "', ' tidak ditemukan di data gabungan RMD. Pastikan categorized_data.csv disalin jika variabel tersebut adalah kategori kustom.')", "\n",
        "} else {", "\n",
        "  group_var_data <- plot_data_full[['", input$homogeneity_cat_var, "']]", "\n",
        "  if (!is.factor(group_var_data) && length(unique(group_var_data)) > 10) {", "\n",
        "    message('Variabel grup dikategorikan otomatis karena non-faktor dan banyak nilai unik.')", "\n",
        "    group_var_data <- cut(group_var_data, breaks = unique(quantile(group_var_data, probs = seq(0, 1, by = 0.25), na.rm = TRUE)), include.lowest = TRUE, dig.lab = 4)", "\n",
        "  }", "\n",
        "  group_var_data <- as.factor(group_var_data)", "\n",
        "  temp_df_homo <- data.frame(val = selected_num_data, group = group_var_data)", "\n",
        "  temp_df_homo <- na.omit(temp_df_homo)", "\n",
        "  if (nrow(temp_df_homo) == 0) {", "\n",
        "    cat('Tidak ada data valid untuk Uji Homogenitas setelah menghilangkan missing values atau variabel grup tidak memiliki cukup data.')", "\n",
        "  } else if (length(unique(temp_df_homo$group)) < 2) {", "\n",
        "    cat('Variabel grup yang dipilih hanya memiliki satu kategori unik setelah pembersihan data. Uji homogenitas memerlukan setidaknya dua kelompok.')", "\n",
        "  } else {", "\n",
        "    levene_test_result <- leveneTest(temp_df_homo$val ~ temp_df_homo$group)", "\n",
        "    print(levene_test_result)", "\n",
        "  }", "\n",
        "}", "\n",
        "```", "\n",
        "**Interpretasi Hasil Uji Homogenitas:**\n",
        "```{r homogeneity-interpretation-text, echo=FALSE}", "\n",
        "selected_num_data <- plot_data_full[['", input$homogeneity_num_var, "']]", "\n",
        "if ('", input$homogeneity_cat_var, "' == 'Pilih Kategori' || !('", input$homogeneity_cat_var, "' %in% names(plot_data_full)) || ncol(plot_data_full) <= 1) {", "\n",
        "  cat('Interpretasi tidak tersedia karena variabel kategorik tidak dipilih atau tidak valid/kosong.')", "\n",
        "} else {", "\n",
        "  group_var_data <- plot_data_full[['", input$homogeneity_cat_var, "']]", "\n",
        "  if (!is.factor(group_var_data) && length(unique(group_var_data)) > 10) {", "\n",
        "    group_var_data <- cut(group_var_data, breaks = unique(quantile(group_var_data, probs = seq(0, 1, by = 0.25), na.rm = TRUE)), include.lowest = TRUE, dig.lab = 4)", "\n",
        "  }", "\n",
        "  group_var_data <- as.factor(group_var_data)", "\n",
        "  temp_df_homo <- data.frame(val = selected_num_data, group = group_var_data)", "\n",
        "  temp_df_homo <- na.omit(temp_df_homo)", "\n",
        "  if (nrow(temp_df_homo) == 0 || length(unique(temp_df_homo$group)) < 2) {", "\n",
        "    cat('Interpretasi tidak tersedia karena data tidak cukup atau variabel grup tidak valid.')", "\n",
        "  } else {", "\n",
        "    levene_test_result <- leveneTest(temp_df_homo$val ~ temp_df_homo$group)", "\n",
        "    p_val <- levene_test_result$`Pr(>F)`[1]", "\n",
        "    cat('Nilai p (', format(round(p_val, 4), scientific = FALSE), ')', if(p_val < 0.05) '<' else '>=', ' 0.05.\\n')", "\n",
        "    if (p_val < 0.05) {", "\n",
        "      cat('Karena nilai p < tingkat signifikansi, kita menolak Hipotesis Nol (H0).\\n')", "\n",
        "      cat('Ini berarti ada bukti statistik yang kuat bahwa varians variabel ', '", input$homogeneity_num_var, "', ' antar kelompok ', '", input$homogeneity_cat_var, "', ' TIDAK homogen.\\n')", "\n",
        "    } else {", "\n",
        "      cat('Karena nilai p >= tingkat signifikansi, kita GAGAL menolak Hipotesis Nol (H0).\\n')", "\n",
        "      cat('Ini berarti tidak ada cukup bukti statistik untuk menyimpulkan bahwa varians variabel ', '", input$homogeneity_num_var, "', ' antar kelompok ', '", input$homogeneity_cat_var, "', ' homogen.\\n')", "\n",
        "    }", "\n",
        "  }", "\n",
        "}", "\n",
        "```\n\n"
      )
      writeChar(rmd_content, file, eos = NULL)
    }
  )
  
  # --- Download Handler untuk Laporan Statistik Inferensia (RMD Output) ---
  output$download_inferensia_report <- downloadHandler(
    filename = function() {
      paste("laporan_statistik_inferensia_", Sys.Date(), ".Rmd", sep = "")
    },
    content = function(file) {
      # --- Persiapan Data untuk RMD ---
      has_categorized_data <- ncol(stored_categorized_data()) > 1
      if (has_categorized_data) {
        # Jika ada data kategori yang disimpan, pastikan file-nya ada untuk RMD
        write.csv(stored_categorized_data(), "categorized_data.csv", row.names = FALSE)
      } else {
        # Hapus file lama jika tidak ada data kategori baru, atau beri peringatan
        if (file.exists("categorized_data.csv")) {
          file.remove("categorized_data.csv")
        }
        showNotification(
          "Peringatan: Tidak ada variabel kategori yang disimpan. Laporan .Rmd mungkin tidak mereplikasi hasil uji statistik yang menggunakan kategori kustom. Untuk menyertakan data kategori, silakan proses data di 'Manajemen Data' -> 'Transformasi Data' dan klik 'Simpan Data Kategori' terlebih dahulu.",
          type = "warning",
          duration = 8
        )
      }
      
      # --- Buat konten RMarkdown secara dinamis ---
      rmd_content <- paste0(
        "---", "\n",
        "title: \"Laporan Statistik Inferensia\"", "\n",
        "author: \"Shiny App Analisis Data SoVI\"", "\n",
        "date: \"", Sys.Date(), "\"", "\n",
        "output: html_document", "\n",
        "---", "\n\n",
        
        "```{r setup, include=FALSE}", "\n",
        "knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 6)", "\n",
        "library(dplyr)", "\n",
        "library(ggplot2)", "\n",
        "library(nortest) # Untuk ad.test (jika digunakan internal oleh uji lain)", "\n",
        "library(car) # Untuk leveneTest (jika digunakan internal oleh uji lain)", "\n",
        "# PASTIKAN FILE 'sovi_data.csv' DAN 'categorized_data.csv' (jika ada) BERADA DI DIREKTORI YANG SAMA DENGAN FILE RMD INI.", "\n",
        "sovi_data <- read.csv('sovi_data.csv')", "\n",
        "stored_categorized_data <- data.frame(DISTRICTCODE = character(0))", "\n",
        "tryCatch({", "\n",
        "  if (file.exists('categorized_data.csv')) {", "\n",
        "    temp_cat_data <- read.csv('categorized_data.csv')", "\n",
        "    if (ncol(temp_cat_data) > 1 && all(sapply(temp_cat_data, class) %in% c('numeric', 'integer', 'factor', 'character'))) {", "\n",
        "      stored_categorized_data <- temp_cat_data", "\n",
        "    }", "\n",
        "  }", "\n",
        "}, error = function(e) {", "\n",
        "  message('File categorized_data.csv tidak ditemukan atau tidak dapat dimuat: ', e$message)", "\n",
        "})", "\n",
        "plot_data_full <- left_join(sovi_data, stored_categorized_data, by = 'DISTRICTCODE')", "\n",
        "```", "\n\n",
        
        "## Statistik Inferensia", "\n\n",
        
        # --- Uji-t Satu Sampel ---
        "### Uji-t Satu Sampel", "\n",
        "**Variabel Numerik:** `", input$one_sample_t_var, "`\n",
        "**Nilai Hipotesis (μ0):** `", input$one_sample_t_mu, "`\n",
        "**Hipotesis Alternatif:** `", input$one_sample_t_alt, "`\n\n",
        "```{r one-sample-t-test}", "\n",
        "data_var <- sovi_data[['", input$one_sample_t_var, "']]", "\n",
        "data_var <- na.omit(data_var)", "\n",
        "if (length(data_var) < 2) {", "\n",
        "  cat('Error: Variabel tidak memiliki cukup data (minimal 2 observasi setelah menghilangkan NA).')", "\n",
        "} else {", "\n",
        "  result <- t.test(data_var, mu = ", input$one_sample_t_mu, ", alternative = '", input$one_sample_t_alt, "')", "\n",
        "  print(result)", "\n",
        "}", "\n",
        "```", "\n",
        "**Interpretasi Uji-t Satu Sampel:**\n",
        "```{r one-sample-t-interpret, echo=FALSE}", "\n",
        "data_var <- sovi_data[['", input$one_sample_t_var, "']]", "\n",
        "data_var <- na.omit(data_var)", "\n",
        "if (length(data_var) < 2) {", "\n",
        "  cat('Interpretasi tidak tersedia karena data tidak cukup.')", "\n",
        "} else {", "\n",
        "  result <- t.test(data_var, mu = ", input$one_sample_t_mu, ", alternative = '", input$one_sample_t_alt, "')", "\n",
        "  p_val <- result$p.value", "\n",
        "  h0_text <- paste0('Rata-rata populasi (μ) sama dengan ', ", input$one_sample_t_mu, ", '.')", "\n",
        "  h1_text <- switch('", input$one_sample_t_alt, "',", "\n",
        "                    'two.sided' = paste0('Rata-rata populasi (μ) tidak sama dengan ', ", input$one_sample_t_mu, ", '.'),", "\n",
        "                    'less' = paste0('Rata-rata populasi (μ) kurang dari ', ", input$one_sample_t_mu, ", '.'),", "\n",
        "                    'greater' = paste0('Rata-rata populasi (μ) lebih dari ', ", input$one_sample_t_mu, ", '.'))", "\n",
        "  cat('Hipotesis Nol (H0): ', h0_text, '\\n')", "\n",
        "  cat('Hipotesis Alternatif (H1): ', h1_text, '\\n')", "\n",
        "  cat('Nilai p (p-value): ', format(round(p_val, 4), scientific = FALSE), '.\\n')", "\n",
        "  if (p_val < 0.05) {", "\n",
        "    cat('Karena nilai p < 0.05, kita menolak H0.\\n')", "\n",
        "    cat('Ini berarti ada bukti statistik yang signifikan untuk menyimpulkan bahwa ', h1_text, ' pada tingkat signifikansi 5%.')", "\n",
        "  } else {", "\n",
        "    cat('Karena nilai p >= 0.05, kita gagal menolak H0.\\n')", "\n",
        "    cat('Ini berarti tidak ada cukup bukti statistik untuk menyimpulkan bahwa ', h1_text, ' pada tingkat signifikansi 5%.')", "\n",
        "  }", "\n",
        "}", "\n",
        "```\n\n",
        
        # --- Uji-t Dua Sampel ---
        "### Uji-t Dua Sampel", "\n",
        "**Variabel Numerik:** `", input$two_sample_t_num_var, "`\n",
        "**Variabel Kategorik (Grup):** `", input$two_sample_t_cat_var, "`\n",
        "**Jenis Uji:** `", input$two_sample_t_type, "`\n",
        "**Hipotesis Alternatif:** `", input$two_sample_t_alt, "`\n\n",
        "```{r two-sample-t-test}", "\n",
        "if ('", input$two_sample_t_cat_var, "' == 'Pilih Kategori') {", "\n",
        "  cat('Silakan pilih variabel kategorik untuk uji 2 kelompok.')", "\n",
        "} else if (!('", input$two_sample_t_cat_var, "' %in% names(plot_data_full))) {", "\n",
        "  cat('Error: Variabel kategorik ', '", input$two_sample_t_cat_var, "', ' tidak ditemukan di data gabungan RMD. Pastikan categorized_data.csv disalin jika variabel tersebut adalah kategori kustom.')", "\n",
        "} else {", "\n",
        "  num_var <- plot_data_full[['", input$two_sample_t_num_var, "']]", "\n",
        "  cat_var <- as.factor(plot_data_full[['", input$two_sample_t_cat_var, "']])", "\n",
        "  if (nlevels(cat_var) != 2) {", "\n",
        "    cat('Error: Variabel kategorik harus memiliki tepat 2 level/kelompok untuk uji t 2 kelompok.')", "\n",
        "  } else {", "\n",
        "    valid_data <- data.frame(num = num_var, cat = cat_var)", "\n",
        "    valid_data <- na.omit(valid_data)", "\n",
        "    if (nrow(valid_data) < 2) {", "\n",
        "      cat('Error: Variabel tidak memiliki cukup data setelah menghilangkan NA dan memfilter 2 kelompok.')", "\n",
        "    } else if (any(table(valid_data$cat) < 1)) {", "\n",
        "      cat('Error: Setidaknya satu kelompok memiliki kurang dari 1 observasi setelah pembersihan data.')", "\n",
        "    } else {", "\n",
        "      var_equal <- ('", input$two_sample_t_type, "' == 'independent_equal_var')", "\n",
        "      result <- t.test(valid_data$num ~ valid_data$cat, var.equal = var_equal, alternative = '", input$two_sample_t_alt, "')", "\n",
        "      print(result)", "\n",
        "    }", "\n",
        "  }", "\n",
        "}", "\n",
        "```", "\n",
        "**Interpretasi Uji-t Dua Sampel:**\n",
        "```{r two-sample-t-interpret, echo=FALSE}", "\n",
        "if ('", input$two_sample_t_cat_var, "' == 'Pilih Kategori' || !('", input$two_sample_t_cat_var, "' %in% names(plot_data_full))) {", "\n",
        "  cat('Interpretasi tidak tersedia karena variabel kategorik tidak dipilih atau tidak valid/kosong.')", "\n",
        "} else {", "\n",
        "  num_var <- plot_data_full[['", input$two_sample_t_num_var, "']]", "\n",
        "  cat_var <- as.factor(plot_data_full[['", input$two_sample_t_cat_var, "']])", "\n",
        "  if (nlevels(cat_var) != 2) {", "\n",
        "    cat('Interpretasi tidak tersedia: Variabel kategorik harus memiliki tepat 2 level.')", "\n",
        "  } else {", "\n",
        "    valid_data <- data.frame(num = num_var, cat = cat_var)", "\n",
        "    valid_data <- na.omit(valid_data)", "\n",
        "    if (nrow(valid_data) < 2 || any(table(valid_data$cat) < 1)) {", "\n",
        "      cat('Interpretasi tidak tersedia karena data tidak cukup atau tidak valid.')", "\n",
        "    } else {", "\n",
        "      var_equal <- ('", input$two_sample_t_type, "' == 'independent_equal_var')", "\n",
        "      result <- t.test(valid_data$num ~ valid_data$cat, var.equal = var_equal, alternative = '", input$two_sample_t_alt, "')", "\n",
        "      p_val <- result$p.value", "\n",
        "      group_names <- levels(valid_data$cat)", "\n",
        "      h0_text <- paste0('Rata-rata populasi ', group_names[1], ' sama dengan rata-rata populasi ', group_names[2], '.')", "\n",
        "      h1_text <- switch('", input$two_sample_t_alt, "',", "\n",
        "                        'two.sided' = paste0('Rata-rata populasi ', group_names[1], ' tidak sama dengan rata-rata populasi ', group_names[2], '.'),", "\n",
        "                        'less' = paste0('Rata-rata populasi ', group_names[1], ' kurang dari rata-rata populasi ', group_names[2], '.'),", "\n",
        "                        'greater' = paste0('Rata-rata populasi ', group_names[1], ' lebih dari rata-rata populasi ', group_names[2], '.')", "\n",
        "      )", "\n",
        "      cat('Hipotesis Nol (H0): ', h0_text, '\\n')", "\n",
        "      cat('Hipotesis Alternatif (H1): ', h1_text, '\\n')", "\n",
        "      cat('Jenis Uji t-test: ', '", input$two_sample_t_type, "', '.\\n')", "\n",
        "      cat('Nilai p (p-value): ', format(round(p_val, 4), scientific = FALSE), '.\\n')", "\n",
        "      if (p_val < 0.05) {", "\n",
        "        cat('Karena nilai p < 0.05, kita menolak H0.\\n')", "\n",
        "        cat('Ini berarti ada bukti statistik yang signifikan untuk menyimpulkan bahwa ', h1_text, ' pada tingkat signifikansi 5%.')", "\n",
        "      } else {", "\n",
        "        cat('Karena nilai p >= 0.05, kita gagal menolak H0.\\n')", "\n",
        "        cat('Ini berarti tidak ada cukup bukti statistik untuk menyimpulkan bahwa ', h1_text, ' pada tingkat signifikansi 5%.')", "\n",
        "      }", "\n",
        "    }", "\n",
        "}", "\n",
        "```\n\n",
        
        # --- Uji Proporsi ---
        "### Uji Proporsi", "\n",
        "**Jenis Uji Proporsi:** `", input$prop_test_type, "`\n",
        "```{r prop-test}", "\n",
        "result <- NULL", "\n",
        "if ('", input$prop_test_type, "' == '1 Kelompok') {", "\n",
        "  if (", input$prop_1_x, " > ", input$prop_1_n, ") {", "\n",
        "    cat('Error: Jumlah kejadian (x) tidak boleh lebih besar dari total observasi (n).')", "\n",
        "  } else {", "\n",
        "    result <- prop.test(x = ", input$prop_1_x, ", n = ", input$prop_1_n, ", p = ", input$prop_1_p, ", alternative = '", input$prop_1_alt, "')", "\n",
        "    print(result)", "\n",
        "  }", "\n",
        "} else {", "\n",
        "  if (", input$prop_2_x1, " > ", input$prop_2_n1, " || ", input$prop_2_x2, " > ", input$prop_2_n2, ") {", "\n",
        "    cat('Error: Jumlah kejadian (x) tidak boleh lebih besar dari total observasi (n) di salah satu kelompok.')", "\n",
        "  } else {", "\n",
        "    x_vec <- c(", input$prop_2_x1, ", ", input$prop_2_x2, ")", "\n",
        "    n_vec <- c(", input$prop_2_n1, ", ", input$prop_2_n2, ")", "\n",
        "    result <- prop.test(x_vec, n_vec)", "\n",
        "    print(result)", "\n",
        "  }", "\n",
        "}", "\n",
        "```", "\n",
        "**Interpretasi Uji Proporsi:**\n",
        "```{r prop-test-interpret, echo=FALSE}", "\n",
        "result <- NULL", "\n",
        "if ('", input$prop_test_type, "' == '1 Kelompok') {", "\n",
        "  if (", input$prop_1_x, " > ", input$prop_1_n, ") {", "\n",
        "    cat('Interpretasi tidak tersedia karena input tidak valid.')", "\n",
        "  } else {", "\n",
        "    result <- prop.test(x = ", input$prop_1_x, ", n = ", input$prop_1_n, ", p = ", input$prop_1_p, ", alternative = '", input$prop_1_alt, "')", "\n",
        "    p_val <- result$p.value", "\n",
        "    h0_text <- paste0('Proporsi populasi (p) sama dengan ', ", input$prop_1_p, ", '.')", "\n",
        "    h1_text <- switch('", input$prop_1_alt, "',", "\n",
        "                      'two.sided' = paste0('Proporsi populasi (p) tidak sama dengan ', ", input$prop_1_p, ", '.'),", "\n",
        "                      'less' = paste0('Proporsi populasi (p) kurang dari ', ", input$prop_1_p, ", '.'),", "\n",
        "                      'greater' = paste0('Proporsi populasi (p) lebih dari ', ", input$prop_1_p, ", '.'))", "\n",
        "    cat('Hipotesis Nol (H0): ', h0_text, '\\n')", "\n",
        "    cat('Hipotesis Alternatif (H1): ', h1_text, '\\n')", "\n",
        "    cat('Jenis Uji: Uji Proporsi 1 Kelompok.\\n')", "\n",
        "    cat('Nilai p (p-value): ', format(round(p_val, 4), scientific = FALSE), '.\\n')", "\n",
        "    if (p_val < 0.05) {", "\n",
        "      cat('Karena nilai p < 0.05, kita menolak H0.\\n')", "\n",
        "      cat('Ini berarti ada bukti statistik yang signifikan bahwa ', h1_text, ' pada tingkat signifikansi 5%.')", "\n",
        "    } else {", "\n",
        "      cat('Karena nilai p >= 0.05, kita gagal menolak H0.\\n')", "\n",
        "      cat('Ini berarti tidak ada cukup bukti statistik untuk menyimpulkan bahwa ', h1_text, ' pada tingkat signifikansi 5%.')", "\n",
        "    }", "\n",
        "  }", "\n",
        "} else {", "\n",
        "  if (", input$prop_2_x1, " > ", input$prop_2_n1, " || ", input$prop_2_x2, " > ", input$prop_2_n2, ") {", "\n",
        "    cat('Interpretasi tidak tersedia karena input tidak valid.')", "\n",
        "  } else {", "\n",
        "    x_vec <- c(", input$prop_2_x1, ", ", input$prop_2_x2, ")", "\n",
        "    n_vec <- c(", input$prop_2_n1, ", ", input$prop_2_n2, ")", "\n",
        "    result <- prop.test(x_vec, n_vec)", "\n",
        "    p_val <- result$p.value", "\n",
        "    h0_text <- 'Proporsi kedua populasi adalah sama (p1 = p2).'", "\n",
        "    h1_text <- 'Proporsi kedua populasi tidak sama (p1 ≠ p2).'", "\n",
        "    cat('Hipotesis Nol (H0): ', h0_text, '\\n')", "\n",
        "    cat('Hipotesis Alternatif (H1): ', h1_text, '\\n')", "\n",
        "    cat('Jenis Uji: Uji Proporsi 2 Kelompok.\\n')", "\n",
        "    cat('Nilai p (p-value): ', format(round(p_val, 4), scientific = FALSE), '.\\n')", "\n",
        "    if (p_val < 0.05) {", "\n",
        "      cat('Karena nilai p < 0.05, kita menolak H0.\\n')", "\n",
        "      cat('Ini berarti ada bukti statistik yang signifikan bahwa ', h1_text, ' pada tingkat signifikansi 5%.')", "\n",
        "    } else {", "\n",
        "      cat('Karena nilai p >= 0.05, kita gagal menolak H0.\\n')", "\n",
        "      cat('Ini berarti tidak ada cukup bukti statistik untuk menyimpulkan bahwa ', h1_text, ' pada tingkat signifikansi 5%.')", "\n",
        "    }", "\n",
        "  }", "\n",
        "}", "\n",
        "```\n\n",
        
        # --- Uji Varians (F-Test) ---
        "### Uji Varians (F-Test)", "\n",
        "**Variabel Numerik:** `", input$var_test_num_var, "`\n",
        "**Variabel Kategorik (Grup):** `", input$var_test_cat_var, "`\n",
        "**Hipotesis Alternatif:** `", input$var_test_alt, "`\n\n",
        "```{r var-test}", "\n",
        "if ('", input$var_test_cat_var, "' == 'Pilih Kategori') {", "\n",
        "  cat('Silakan pilih variabel kategorik (grup) untuk uji varians.')", "\n",
        "} else if (!('", input$var_test_cat_var, "' %in% names(plot_data_full))) {", "\n",
        "  cat('Error: Variabel kategorik ', '", input$var_test_cat_var, "', ' tidak ditemukan di data gabungan RMD. Pastikan categorized_data.csv disalin jika variabel tersebut adalah kategori kustom.')", "\n",
        "} else {", "\n",
        "  num_var <- plot_data_full[['", input$var_test_num_var, "']]", "\n",
        "  cat_var <- as.factor(plot_data_full[['", input$var_test_cat_var, "']])", "\n",
        "  if (nlevels(cat_var) != 2) {", "\n",
        "    cat('Error: Variabel kategorik harus memiliki tepat 2 level/kelompok untuk uji varians 2 kelompok.')", "\n",
        "  } else {", "\n",
        "    valid_data <- data.frame(num = num_var, cat = cat_var)", "\n",
        "    valid_data <- na.omit(valid_data)", "\n",
        "    if (nrow(valid_data) < 2 || any(table(valid_data$cat) < 2)) {", "\n",
        "      cat('Error: Variabel tidak memiliki cukup data (minimal 2 observasi per grup setelah menghilangkan NA dan memfilter 2 kelompok).')", "\n",
        "    } else {", "\n",
        "      result <- var.test(valid_data$num ~ valid_data$cat, alternative = '", input$var_test_alt, "')", "\n",
        "      print(result)", "\n",
        "    }", "\n",
        "  }", "\n",
        "}", "\n",
        "```", "\n",
        "**Interpretasi Uji Varians:**\n",
        "```{r var-test-interpret, echo=FALSE}", "\n",
        "if ('", input$var_test_cat_var, "' == 'Pilih Kategori' || !('", input$var_test_cat_var, "' %in% names(plot_data_full))) {", "\n",
        "  cat('Interpretasi tidak tersedia karena variabel kategorik tidak dipilih atau tidak valid/kosong.')", "\n",
        "} else {", "\n",
        "  num_var <- plot_data_full[['", input$var_test_num_var, "']]", "\n",
        "  cat_var <- as.factor(plot_data_full[['", input$var_test_cat_var, "']])", "\n",
        "  if (nlevels(cat_var) != 2) {", "\n",
        "    cat('Interpretasi tidak tersedia: Variabel kategorik harus memiliki tepat 2 level.')", "\n",
        "  } else {", "\n",
        "    valid_data <- data.frame(num = num_var, cat = cat_var)", "\n",
        "    valid_data <- na.omit(valid_data)", "\n",
        "    if (nrow(valid_data) < 2 || any(table(valid_data$cat) < 2)) {", "\n",
        "      cat('Interpretasi tidak tersedia karena data tidak cukup atau tidak valid.')", "\n",
        "    } else {", "\n",
        "      result <- var.test(valid_data$num ~ valid_data$cat, alternative = '", input$var_test_alt, "')", "\n",
        "      p_val <- result$p.value", "\n",
        "      group_names <- levels(valid_data$cat)", "\n",
        "      h0_text <- paste0('Varians populasi ', group_names[1], ' sama dengan varians populasi ', group_names[2], '.')", "\n",
        "      h1_text <- switch('", input$var_test_alt, "',", "\n",
        "                        'two.sided' = paste0('Varians populasi ', group_names[1], ' tidak sama dengan varians populasi ', group_names[2], '.'),", "\n",
        "                        'less' = paste0('Varians populasi ', group_names[1], ' kurang dari varians populasi ', group_names[2], '.'),", "\n",
        "                        'greater' = paste0('Varians populasi ', group_names[1], ' lebih dari varians populasi ', group_names[2], '.')", "\n",
        "      )", "\n",
        "      cat('Hipotesis Nol (H0): ', h0_text, '\\n')", "\n",
        "      cat('Hipotesis Alternatif (H1): ', h1_text, '\\n')", "\n",
        "      cat('Uji yang Digunakan: F-test (var.test).\\n')", "\n",
        "      cat('Nilai p (p-value): ', format(round(p_val, 4), scientific = FALSE), '.\\n')", "\n",
        "      if (p_val < 0.05) {", "\n",
        "        cat('Karena nilai p < 0.05, kita menolak H0.\\n')", "\n",
        "        cat('Ini berarti ada bukti statistik yang signifikan bahwa ', h1_text, ' pada tingkat signifikansi 5%.')", "\n",
        "      } else {", "\n",
        "        cat('Karena nilai p >= 0.05, kita gagal menolak H0.\\n')", "\n",
        "        cat('Ini berarti tidak ada cukup bukti statistik untuk menyimpulkan bahwa ', h1_text, ' pada tingkat signifikansi 5%.')", "\n",
        "      }", "\n",
        "    }", "\n",
        "}", "\n",
        "```\n\n",
        
        # --- ANOVA ---
        "### Analisis Varians (ANOVA)", "\n",
        "**Jenis ANOVA:** `", input$anova_type, "`\n",
        "**Variabel Dependen (Numerik):** `", if(input$anova_type == 'One-Way ANOVA') input$anova_1w_num_var else input$anova_2w_num_var, "`\n",
        "**Faktor 1:** `", if(input$anova_type == 'One-Way ANOVA') input$anova_1w_cat_var else input$anova_2w_cat_var1, "`\n",
        if (input$anova_type == 'Two-Way ANOVA') paste0("**Faktor 2:** `", input$anova_2w_cat_var2, "`\n") else "",
        if (input$anova_type == 'Two-Way ANOVA' && input$anova_2w_interaction) paste0("**Sertakan Interaksi:** Ya\n") else if (input$anova_type == 'Two-Way ANOVA' && !input$anova_2w_interaction) paste0("**Sertakan Interaksi:** Tidak\n") else "",
        "\n",
        "```{r anova-test}", "\n",
        "num_var_name <- if('", input$anova_type, "' == 'One-Way ANOVA') '", input$anova_1w_num_var, "' else '", input$anova_2w_num_var, "'", "\n",
        "cat_var_name1 <- if('", input$anova_type, "' == 'One-Way ANOVA') '", input$anova_1w_cat_var, "' else '", input$anova_2w_cat_var1, "'", "\n",
        "cat_var_name2 <- if('", input$anova_type, "' == 'Two-Way ANOVA') '", input$anova_2w_cat_var2, "' else NULL", "\n",
        "anova_data_subset <- plot_data_full", "\n",
        "if ('", input$anova_type, "' == 'One-Way ANOVA') {", "\n",
        "  if (cat_var_name1 == 'Pilih Kategori') {", "\n",
        "    cat('Silakan pilih variabel kategorik untuk One-Way ANOVA.')", "\n",
        "  } else if (!(cat_var_name1 %in% names(anova_data_subset))) {", "\n",
        "    cat('Error: Variabel kategorik ', cat_var_name1, ' tidak ditemukan di data gabungan RMD. Pastikan categorized_data.csv disalin jika variabel tersebut adalah kategori kustom.')", "\n",
        "  } else {", "\n",
        "    anova_data_subset[[cat_var_name1]] <- as.factor(anova_data_subset[[cat_var_name1]])", "\n",
        "    clean_anova_data <- anova_data_subset %>% select(all_of(c(num_var_name, cat_var_name1))) %>% na.omit()", "\n",
        "    if (nrow(clean_anova_data) == 0 || nlevels(clean_anova_data[[cat_var_name1]]) < 2 || any(table(clean_anova_data[[cat_var_name1]]) < 1)) {", "\n",
        "      cat('Error: Data tidak cukup atau variabel kategorik tidak valid untuk One-Way ANOVA.')", "\n",
        "    } else {", "\n",
        "      model <- aov(as.formula(paste0(num_var_name, ' ~ ', cat_var_name1)), data = clean_anova_data)", "\n",
        "      print(summary(model))", "\n",
        "    }", "\n",
        "  }", "\n",
        "} else { # Two-Way ANOVA", "\n",
        "  if (cat_var_name1 == 'Pilih Kategori' || cat_var_name2 == 'Pilih Kategori') {", "\n",
        "    cat('Silakan pilih kedua variabel kategorik untuk Two-Way ANOVA.')", "\n",
        "  } else if (!(cat_var_name1 %in% names(anova_data_subset)) || !(cat_var_name2 %in% names(anova_data_subset))) {", "\n",
        "    cat('Error: Satu atau kedua variabel kategorik tidak ditemukan di data gabungan RMD.')", "\n",
        "  } else {", "\n",
        "    anova_data_subset[[cat_var_name1]] <- as.factor(anova_data_subset[[cat_var_name1]])", "\n",
        "    anova_data_subset[[cat_var_name2]] <- as.factor(anova_data_subset[[cat_var_name2]])", "\n",
        "    clean_anova_data <- anova_data_subset %>% select(all_of(c(num_var_name, cat_var_name1, cat_var_name2))) %>% na.omit()", "\n",
        "    if (nrow(clean_anova_data) == 0 || nlevels(clean_anova_data[[cat_var_name1]]) < 2 || nlevels(clean_anova_data[[cat_var_name2]]) < 2 || any(table(clean_anova_data[[cat_var_name1]], clean_anova_data[[cat_var_name2]]) < 1)) {", "\n",
        "      cat('Error: Data tidak cukup atau variabel kategorik tidak valid untuk Two-Way ANOVA.')", "\n",
        "    } else {", "\n",
        "      formula_str <- paste0(num_var_name, ' ~ ', cat_var_name1, ' + ', cat_var_name2)", "\n",
        "      if (", tolower(input$anova_2w_interaction), ") {", "\n", # Assuming input$anova_2w_interaction is boolean
        "        formula_str <- paste0(formula_str, ' + ', cat_var_name1, ':', cat_var_name2)", "\n",
        "      }", "\n",
        "      model <- aov(as.formula(formula_str), data = clean_anova_data)", "\n",
        "      print(summary(model))", "\n",
        "    }", "\n",
        "  }", "\n",
        "}", "\n",
        "```", "\n",
        "**Interpretasi ANOVA:**\n",
        "```{r anova-interpret, echo=FALSE}", "\n",
        "num_var_name <- if('", input$anova_type, "' == 'One-Way ANOVA') '", input$anova_1w_num_var, "' else '", input$anova_2w_num_var, "'", "\n",
        "cat_var_name1 <- if('", input$anova_type, "' == 'One-Way ANOVA') '", input$anova_1w_cat_var, "' else '", input$anova_2w_cat_var1, "'", "\n",
        "cat_var_name2 <- if('", input$anova_type, "' == 'Two-Way ANOVA') '", input$anova_2w_cat_var2, "' else NULL", "\n",
        "anova_data_subset <- plot_data_full", "\n",
        "interpret_lines <- character(0)", "\n",
        "model_valid <- FALSE", "\n", # Flag to check if a valid model was run
        
        "if ('", input$anova_type, "' == 'One-Way ANOVA') {", "\n",
        "  if (cat_var_name1 == 'Pilih Kategori') {", "\n",
        "    interpret_lines <- c(interpret_lines, 'Interpretasi tidak tersedia: Variabel kategorik belum dipilih.')", "\n",
        "  } else if (!(cat_var_name1 %in% names(anova_data_subset))) {", "\n",
        "    interpret_lines <- c(interpret_lines, 'Interpretasi tidak tersedia: Variabel kategorik tidak ditemukan di data gabungan RMD.')", "\n",
        "  } else {", "\n",
        "    anova_data_subset[[cat_var_name1]] <- as.factor(anova_data_subset[[cat_var_name1]])", "\n",
        "    clean_anova_data <- anova_data_subset %>% select(all_of(c(num_var_name, cat_var_name1))) %>% na.omit()", "\n",
        "    if (nrow(clean_anova_data) == 0 || nlevels(clean_anova_data[[cat_var_name1]]) < 2 || any(table(clean_anova_data[[cat_var_name1]]) < 1)) {", "\n",
        "      interpret_lines <- c(interpret_lines, 'Interpretasi tidak tersedia: Data tidak cukup atau variabel kategorik tidak valid untuk One-Way ANOVA.')", "\n",
        "    } else {", "\n",
        "      model <- aov(as.formula(paste0(num_var_name, ' ~ ', cat_var_name1)), data = clean_anova_data)", "\n",
        "      result_summary <- summary(model)", "\n",
        "      p_vals <- result_summary[[1]]$`Pr(>F)`", "\n",
        "      factor_names <- rownames(result_summary[[1]])", "\n",
        "      main_effect_p <- p_vals[factor_names == cat_var_name1]", "\n",
        "      if (!is.null(main_effect_p) && length(main_effect_p) > 0) {", "\n",
        "        if (main_effect_p < 0.05) {", "\n",
        "          interpret_lines <- c(interpret_lines,", "\n",
        "            paste0('Karena nilai p untuk faktor \\'', cat_var_name1, '\\' adalah ', format(round(main_effect_p, 4), scientific = FALSE), ' (< 0.05), kita menolak H0.\\n'),", "\n",
        "            paste0('Ini berarti ada perbedaan rata-rata yang signifikan dari variabel ', num_var_name, ' di antara setidaknya dua kelompok dalam faktor \\'', cat_var_name1, '\\'.')", "\n",
        "          )", "\n",
        "          interpret_lines <- c(interpret_lines, 'Untuk mengetahui kelompok mana yang berbeda, diperlukan uji post-hoc (misalnya Tukey HSD).')", "\n",
        "        } else {", "\n",
        "          interpret_lines <- c(interpret_lines,", "\n",
        "            paste0('Karena nilai p untuk faktor \\'', cat_var_name1, '\\' adalah ', format(round(main_effect_p, 4), scientific = FALSE), ' (>= 0.05), kita gagal menolak H0.\\n'),", "\n",
        "            paste0('Ini berarti tidak ada perbedaan rata-rata yang signifikan dari variabel ', num_var_name, ' di antara kelompok-kelompok dalam faktor \\'', cat_var_name1, '\\'.')", "\n",
        "          )", "\n",
        "        }", "\n",
        "      }", "\n",
        "    }", "\n",
        "} else { # Two-Way ANOVA", "\n",
        "  interpret_lines <- c(interpret_lines, 'Analisis untuk Two-Way ANOVA:')", "\n",
        "  if (cat_var_name1 == 'Pilih Kategori' || cat_var_name2 == 'Pilih Kategori') {", "\n",
        "    interpret_lines <- c(interpret_lines, 'Interpretasi tidak tersedia: Kedua variabel kategorik belum dipilih.')", "\n",
        "  } else if (!(cat_var_name1 %in% names(anova_data_subset)) || !(cat_var_name2 %in% names(anova_data_subset))) {", "\n",
        "    interpret_lines <- c(interpret_lines, 'Interpretasi tidak tersedia: Satu atau kedua variabel kategorik tidak ditemukan di data gabungan RMD.')", "\n",
        "  } else {", "\n",
        "    anova_data_subset[[cat_var_name1]] <- as.factor(anova_data_subset[[cat_var_name1]])", "\n",
        "    anova_data_subset[[cat_var_name2]] <- as.factor(anova_data_subset[[cat_var_name2]])", "\n",
        "    clean_anova_data <- anova_data_subset %>% select(all_of(c(num_var_name, cat_var_name1, cat_var_name2))) %>% na.omit()", "\n",
        "    if (nrow(clean_anova_data) == 0 || nlevels(clean_anova_data[[cat_var_name1]]) < 2 || nlevels(clean_anova_data[[cat_var_name2]]) < 2 || any(table(clean_anova_data[[cat_var_name1]], clean_anova_data[[cat_var_name2]]) < 1)) {", "\n",
        "      interpret_lines <- c(interpret_lines, 'Interpretasi tidak tersedia: Data tidak cukup atau variabel kategorik tidak valid untuk Two-Way ANOVA.')", "\n",
        "    } else {", "\n",
        "      formula_str <- paste0(num_var_name, ' ~ ', cat_var_name1, ' + ', cat_var_name2)", "\n",
        "      if (", tolower(input$anova_2w_interaction), ") {", "\n",
        "        formula_str <- paste0(formula_str, ' + ', cat_var_name1, ':', cat_var_name2)", "\n",
        "      }", "\n",
        "      model <- aov(as.formula(formula_str), data = clean_anova_data)", "\n",
        "      result_summary <- summary(model)", "\n",
        "      p_vals <- result_summary[[1]]$`Pr(>F)`", "\n",
        "      factor_names <- rownames(result_summary[[1]])", "\n",
        "      if (", tolower(input$anova_2w_interaction), ") {", "\n",
        "        interaction_name <- paste0(cat_var_name1, ':', cat_var_name2)", "\n",
        "        interaction_p <- p_vals[grepl(interaction_name, factor_names)]", "\n",
        "        if (!is.null(interaction_p) && length(interaction_p) > 0) {", "\n",
        "          if (interaction_p < 0.05) {", "\n",
        "            interpret_lines <- c(interpret_lines,", "\n",
        "              paste0('Interaksi (', interaction_name, '): p-value = ', format(round(interaction_p, 4), scientific = FALSE), ' (< 0.05). Ada efek interaksi yang signifikan.\\n'),", "\n",
        "              'Ini berarti efek satu faktor terhadap rata-rata variabel dependen bergantung pada level faktor yang lain. Fokus interpretasi pada interaksi.')", "\n",
        "          } else {", "\n",
        "            interpret_lines <- c(interpret_lines,", "\n",
        "              paste0('Interaksi (', interaction_name, '): p-value = ', format(round(interaction_p, 4), scientific = FALSE), ' (>= 0.05). Tidak ada efek interaksi yang signifikan.\\n'),", "\n",
        "              'Ini berarti efek satu faktor terhadap rata-rata variabel dependen tidak bergantung pada level faktor yang lain. Fokus interpretasi pada efek utama.')", "\n",
        "          }", "\n",
        "        }", "\n",
        "      }", "\n",
        "      main_effect1_p <- p_vals[factor_names == cat_var_name1]", "\n",
        "      if (!is.null(main_effect1_p) && length(main_effect1_p) > 0) {", "\n",
        "        if (main_effect1_p < 0.05) {", "\n",
        "          interpret_lines <- c(interpret_lines,", "\n",
        "            paste0('Efek Utama \\'', cat_var_name1, '\\': p-value = ', format(round(main_effect1_p, 4), scientific = FALSE), ' (< 0.05). Efek signifikan.\\n'),", "\n",
        "            paste0('Ada perbedaan rata-rata yang signifikan dari variabel ', num_var_name, ' di antara level faktor \\'', cat_var_name1, '\\'.'))", "\n",
        "        } else {", "\n",
        "          interpret_lines <- c(interpret_lines,", "\n",
        "            paste0('Efek Utama \\'', cat_var_name1, '\\': p-value = ', format(round(main_effect1_p, 4), scientific = FALSE), ' (>= 0.05). Efek tidak signifikan.')", "\n",
        "          )", "\n",
        "        }", "\n",
        "      }", "\n",
        "      main_effect2_p <- p_vals[factor_names == cat_var_name2]", "\n",
        "      if (!is.null(main_effect2_p) && length(main_effect2_p) > 0) {", "\n",
        "        if (main_effect2_p < 0.05) {", "\n",
        "          interpret_lines <- c(interpret_lines,", "\n",
        "            paste0('Efek Utama \\'', cat_var_name2, '\\': p-value = ', format(round(main_effect2_p, 4), scientific = FALSE), ' (< 0.05). Efek signifikan.\\n'),", "\n",
        "            paste0('Ada perbedaan rata-rata yang signifikan dari variabel ', num_var_name, ' di antara level faktor \\'', cat_var_name2, '\\'.'))", "\n",
        "        } else {", "\n",
        "          interpret_lines <- c(interpret_lines,", "\n",
        "            paste0('Efek Utama \\'', cat_var_name2, '\\': p-value = ', format(round(main_effect2_p, 4), scientific = FALSE), ' (>= 0.05). Efek tidak signifikan.')", "\n",
        "          )", "\n",
        "        }", "\n",
        "      }", "\n",
        "    }", "\n",
        "}", "\n",
        "cat(paste(interpret_lines, collapse = '\\n'))", "\n",
        "```\n\n"
      )
      writeChar(rmd_content, file, eos = NULL)
    }
  )
  
  # --- Download Handler untuk Laporan Regresi Linear Berganda (RMD Output) ---
  output$download_regresi_report <- downloadHandler(
    filename = function() {
      paste("laporan_regresi_linear_berganda_", Sys.Date(), ".Rmd", sep = "")
    },
    content = function(file) {
      # Validasi input sebelum membuat laporan
      req(input$reg_dependent_var)
      if (is.null(input$reg_independent_vars) || length(input$reg_independent_vars) == 0) {
        showNotification("Peringatan: Tidak ada variabel independen yang dipilih. Laporan tidak akan dibuat.", type = "error", duration = 8)
        stop("Tidak ada variabel independen yang dipilih untuk regresi.")
      }
      
      # --- Persiapan Data untuk RMD ---
      has_categorized_data <- ncol(stored_categorized_data()) > 1
      if (has_categorized_data) {
        write.csv(stored_categorized_data(), "categorized_data.csv", row.names = FALSE)
      } else {
        if (file.exists("categorized_data.csv")) {
          file.remove("categorized_data.csv")
        }
        showNotification(
          "Peringatan: Tidak ada variabel kategori yang disimpan. Laporan .Rmd mungkin tidak mereplikasi hasil regresi yang menggunakan kategori kustom. Untuk menyertakan data kategori, silakan proses data di 'Manajemen Data' -> 'Transformasi Data' dan klik 'Simpan Data Kategori' terlebih dahulu.",
          type = "warning",
          duration = 8
        )
      }
      
      # --- Buat konten RMarkdown secara dinamis ---
      rmd_content <- paste0(
        "---", "\n",
        "title: \"Laporan Regresi Linear Berganda\"", "\n",
        "author: \"Shiny App Analisis Data SoVI\"", "\n",
        "date: \"", Sys.Date(), "\"", "\n",
        "output: html_document", "\n",
        "---", "\n\n",
        
        "```{r setup, include=FALSE}", "\n",
        "knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 6)", "\n",
        "library(dplyr)", "\n",
        "library(ggplot2)", "\n",
        "library(rmarkdown)", "\n",
        "library(car) # Untuk ncvTest, durbinWatsonTest, vif", "\n",
        "library(nortest) # Untuk ad.test", "\n",
        "# PASTIKAN FILE 'sovi_data.csv' DAN 'categorized_data.csv' (jika ada) BERADA DI DIREKTORI YANG SAMA DENGAN FILE RMD INI.", "\n",
        "sovi_data <- read.csv('sovi_data.csv')", "\n",
        "stored_categorized_data <- data.frame(DISTRICTCODE = character(0))", "\n",
        "tryCatch({", "\n",
        "  if (file.exists('categorized_data.csv')) {", "\n",
        "    temp_cat_data <- read.csv('categorized_data.csv')", "\n",
        "    if (ncol(temp_cat_data) > 1 && all(sapply(temp_cat_data, class) %in% c('numeric', 'integer', 'factor', 'character'))) {", "\n",
        "      stored_categorized_data <- temp_cat_data", "\n",
        "    }", "\n",
        "  }", "\n",
        "}, error = function(e) {", "\n",
        "  message('File categorized_data.csv tidak ditemukan atau tidak dapat dimuat: ', e$message)", "\n",
        "})", "\n",
        "reg_data_full <- left_join(sovi_data, stored_categorized_data, by = 'DISTRICTCODE')", "\n",
        "\n",
        "# Replikasi all_categorical_vars_reactive untuk RMD", "\n",
        "all_categorical_vars_in_rmd <- names(sovi_data)[sapply(sovi_data, function(x) { is.numeric(x) && length(unique(x)) < 10 && length(unique(x)) > 1 })]", "\n",
        "if (ncol(stored_categorized_data) > 1) {", "\n",
        "  transformed_cat_cols_rmd <- names(stored_categorized_data)", "\n",
        "  transformed_cat_cols_rmd <- transformed_cat_cols_rmd[transformed_cat_cols_rmd != 'DISTRICTCODE']", "\n",
        "  all_categorical_vars_in_rmd <- unique(c(all_categorical_vars_in_rmd, transformed_cat_cols_rmd))", "\n",
        "}", "\n",
        "```", "\n\n",
        
        "## Regresi Linear Berganda", "\n\n",
        
        "### Konfigurasi Model Regresi", "\n",
        "**Variabel Dependen (Y):** `", input$reg_dependent_var, "`\n",
        "**Variabel Independen (X):** `", paste(input$reg_independent_vars, collapse = ", "), "`\n\n",
        
        "### Output Ringkasan Model Regresi", "\n",
        "```{r regression-model}", "\n",
        "dep_var <- '", input$reg_dependent_var, "'", "\n",
        "indep_vars <- c('", paste(input$reg_independent_vars, collapse = "', '"), "')", "\n",
        "model_vars_names <- c(dep_var, indep_vars)", "\n",
        "clean_reg_data <- reg_data_full %>% select(all_of(model_vars_names))", "\n",
        "for (iv in indep_vars) {", "\n",
        "  if (iv %in% all_categorical_vars_in_rmd && !is.factor(clean_reg_data[[iv]])) {", "\n",
        "    clean_reg_data[[iv]] <- as.factor(clean_reg_data[[iv]])", "\n",
        "  }", "\n",
        "}", "\n",
        "clean_reg_data <- na.omit(clean_reg_data)", "\n",
        "\n",
        "if (nrow(clean_reg_data) == 0) {", "\n",
        "  cat('Error: Tidak ada data valid untuk menjalankan regresi setelah menghilangkan missing values.')", "\n",
        "} else {", "\n",
        "  formula_str <- paste0(dep_var, ' ~ ', paste(indep_vars, collapse = ' + '))", "\n",
        "  model <- lm(as.formula(formula_str), data = clean_reg_data)", "\n",
        "  print(summary(model))", "\n",
        "}", "\n",
        "```", "\n",
        
        "### Interpretasi Model Regresi", "\n",
        "```{r regression-interpret, echo=FALSE}", "\n",
        "dep_var <- '", input$reg_dependent_var, "'", "\n",
        "indep_vars <- c('", paste(input$reg_independent_vars, collapse = "', '"), "')", "\n",
        "model_vars_names <- c(dep_var, indep_vars)", "\n",
        "clean_reg_data <- reg_data_full %>% select(all_of(model_vars_names))", "\n",
        "for (iv in indep_vars) {", "\n",
        "  if (iv %in% all_categorical_vars_in_rmd && !is.factor(clean_reg_data[[iv]])) {", "\n",
        "    clean_reg_data[[iv]] <- as.factor(clean_reg_data[[iv]])", "\n",
        "  }", "\n",
        "}", "\n",
        "clean_reg_data <- na.omit(clean_reg_data)", "\n",
        "\n",
        "if (nrow(clean_reg_data) == 0) {", "\n",
        "  cat('Interpretasi tidak tersedia karena data tidak cukup atau model tidak dapat dijalankan.')", "\n",
        "} else {", "\n",
        "  formula_str <- paste0(dep_var, ' ~ ', paste(indep_vars, collapse = ' + '))", "\n",
        "  model <- lm(as.formula(formula_str), data = clean_reg_data)", "\n",
        "  model_summary <- summary(model)", "\n",
        "  interpret_lines <- character(0)", "\n",
        "  f_stat <- model_summary$fstatistic[1]", "\n",
        "  f_p_val <- pf(f_stat, model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)", "\n",
        "  r_squared <- model_summary$r.squared", "\n",
        "  adj_r_squared <- model_summary$adj.r.squared", "\n",
        "  cat(paste0('Model ini menjelaskan sekitar ', round(r_squared * 100, 2), '% varians dari variabel dependen (R-squared). R-squared yang disesuaikan adalah ', round(adj_r_squared * 100, 2), '%.\\n'))", "\n",
        "  cat(paste0('Uji F model secara keseluruhan memiliki p-value = ', format(round(f_p_val, 4), scientific = FALSE), '.\\n'))", "\n",
        "  if (f_p_val < 0.05) {", "\n",
        "    cat('Karena p-value F-test < 0.05, model regresi secara keseluruhan signifikan secara statistik. Ini berarti setidaknya satu variabel independen memiliki hubungan linear yang signifikan dengan variabel dependen.\\n')", "\n",
        "  } else {", "\n",
        "    cat('Karena p-value F-test >= 0.05, model regresi secara keseluruhan tidak signifikan secara statistik.\\n')", "\n",
        "  }", "\n",
        "  cat('\\nInterpretasi Koefisien Variabel Independen:\\n')", "\n",
        "  coefs <- as.data.frame(model_summary$coefficients)", "\n",
        "  for (i in 2:nrow(coefs)) {", "\n",
        "    var_name <- rownames(coefs)[i]", "\n",
        "    estimate <- round(coefs[i, 'Estimate'], 4)", "\n",
        "    p_val_coef <- coefs[i, 'Pr(>|t|)']", "\n",
        "    if (p_val_coef < 0.05) {", "\n",
        "      cat(paste0('Koefisien untuk \\'', var_name, '\\' adalah ', estimate, ' (p-value = ', format(round(p_val_coef, 4), scientific = FALSE), '). Ini signifikan secara statistik.\\n'))", "\n",
        "      cat(paste0('   Artinya, setiap peningkatan satu unit pada ', var_name, ' diasosiasikan dengan perubahan rata-rata ', estimate, ' unit pada ', dep_var, ', dengan mengontrol variabel lain.\\n'))", "\n",
        "    } else {", "\n",
        "      cat(paste0('Koefisien untuk \\'', var_name, '\\' adalah ', estimate, ' (p-value = ', format(round(p_val_coef, 4), scientific = FALSE), '). Ini tidak signifikan secara statistik.\\n'))", "\n",
        "      cat(paste0('   Artinya, tidak ada bukti statistik yang cukup untuk menyimpulkan bahwa ', var_name, ' memiliki hubungan linear yang signifikan dengan ', dep_var, ', dengan mengontrol variabel lain.\\n'))", "\n",
        "    }", "\n",
        "  }", "\n",
        "}", "\n",
        "```\n\n",
        
        "### Uji Asumsi Regresi Linear", "\n",
        
        "#### 1. Asumsi Linearitas", "\n",
        "Uji yang Digunakan: Inspeksi Visual Plot Residuals vs Fitted. Asumsi linearitas mengasumsikan hubungan linear antara variabel dependen dan independen.\n",
        "Interpretasi: Jika asumsi terpenuhi, titik-titik (residuals) harus tersebar secara acak di sekitar garis horizontal y=0 tanpa pola yang jelas (misalnya, bentuk U, kerucut, atau kurva). Garis biru (lowess) harus mendekati garis merah (y=0). Pola yang jelas mengindikasikan pelanggaran asumsi linearitas.\n\n",
        "```{r linearity-plot, fig.height=6, fig.width=8}", "\n",
        "dep_var <- '", input$reg_dependent_var, "'", "\n",
        "indep_vars <- c('", paste(input$reg_independent_vars, collapse = "', '"), "')", "\n",
        "model_vars_names <- c(dep_var, indep_vars)", "\n",
        "clean_reg_data <- reg_data_full %>% select(all_of(model_vars_names))", "\n",
        "for (iv in indep_vars) {", "\n",
        "  if (iv %in% all_categorical_vars_in_rmd && !is.factor(clean_reg_data[[iv]])) {", "\n",
        "    clean_reg_data[[iv]] <- as.factor(clean_reg_data[[iv]])", "\n",
        "  }", "\n",
        "}", "\n",
        "clean_reg_data <- na.omit(clean_reg_data)", "\n",
        "if (nrow(clean_reg_data) == 0) {", "\n",
        "  plot(NULL, xlim=0:1, ylim=0:1, main='Data tidak cukup untuk plot linearitas', xlab='', ylab='', type='n')", "\n",
        "} else {", "\n",
        "  formula_str <- paste0(dep_var, ' ~ ', paste(indep_vars, collapse = ' + '))", "\n",
        "  model <- lm(as.formula(formula_str), data = clean_reg_data)", "\n",
        "  plot(fitted(model), residuals(model),", "\n",
        "       xlab = 'Nilai Prediksi (Fitted Values)',", "\n",
        "       ylab = 'Residuals',", "\n",
        "       main = 'Plot Residuals vs Fitted Values')", "\n",
        "  abline(h = 0, col = 'red', lty = 2)", "\n",
        "  lines(lowess(fitted(model), residuals(model)), col = 'blue', lty = 1)", "\n",
        "}", "\n",
        "```\n\n",
        
        "#### 2. Asumsi Normalitas Residual", "\n",
        "Uji yang Digunakan: Anderson-Darling Test (ad.test dari paket `nortest`). Asumsi normalitas residual mengasumsikan bahwa error (residual) model terdistribusi normal.\n",
        "Hipotesis Nol (H0): Residual berdistribusi normal.\n",
        "Hipotesis Alternatif (H1): Residual tidak berdistribusi normal.\n",
        "Aturan Keputusan: Tolak H0 jika nilai p < tingkat signifikansi (misalnya 0.05).\n\n",
        "```{r normality-residuals-test}", "\n",
        "dep_var <- '", input$reg_dependent_var, "'", "\n",
        "indep_vars <- c('", paste(input$reg_independent_vars, collapse = "', '"), "')", "\n",
        "model_vars_names <- c(dep_var, indep_vars)", "\n",
        "clean_reg_data <- reg_data_full %>% select(all_of(model_vars_names))", "\n",
        "for (iv in indep_vars) {", "\n",
        "  if (iv %in% all_categorical_vars_in_rmd && !is.factor(clean_reg_data[[iv]])) {", "\n",
        "    clean_reg_data[[iv]] <- as.factor(clean_reg_data[[iv]])", "\n",
        "  }", "\n",
        "}", "\n",
        "clean_reg_data <- na.omit(clean_reg_data)", "\n",
        "if (nrow(clean_reg_data) == 0) {", "\n",
        "  cat('Error: Data tidak cukup untuk uji normalitas residual.')", "\n",
        "} else {", "\n",
        "  formula_str <- paste0(dep_var, ' ~ ', paste(indep_vars, collapse = ' + '))", "\n",
        "  model <- lm(as.formula(formula_str), data = clean_reg_data)", "\n",
        "  res <- residuals(model)", "\n",
        "  if (length(unique(res)) < 5) {", "\n",
        "    cat('Error: Residual tidak memiliki cukup nilai unik untuk uji normalitas.')", "\n",
        "  } else {", "\n",
        "    ad.test(res)", "\n",
        "  }", "\n",
        "}", "\n",
        "```", "\n",
        "**Interpretasi Normalitas Residual:**\n",
        "```{r normality-residuals-interpret, echo=FALSE}", "\n",
        "dep_var <- '", input$reg_dependent_var, "'", "\n",
        "indep_vars <- c('", paste(input$reg_independent_vars, collapse = "', '"), "')", "\n",
        "model_vars_names <- c(dep_var, indep_vars)", "\n",
        "clean_reg_data <- reg_data_full %>% select(all_of(model_vars_names))", "\n",
        "for (iv in indep_vars) {", "\n",
        "  if (iv %in% all_categorical_vars_in_rmd && !is.factor(clean_reg_data[[iv]])) {", "\n",
        "    clean_reg_data[[iv]] <- as.factor(clean_reg_data[[iv]])", "\n",
        "  }", "\n",
        "}", "\n",
        "clean_reg_data <- na.omit(clean_reg_data)", "\n",
        "if (nrow(clean_reg_data) == 0) {", "\n",
        "  cat('Interpretasi tidak tersedia karena data tidak cukup.')", "\n",
        "} else {", "\n",
        "  formula_str <- paste0(dep_var, ' ~ ', paste(indep_vars, collapse = ' + '))", "\n",
        "  model <- lm(as.formula(formula_str), data = clean_reg_data)", "\n",
        "  res <- residuals(model)", "\n",
        "  if (length(unique(res)) < 5) {", "\n",
        "    cat('Tidak cukup nilai unik pada residual untuk melakukan uji normalitas.')", "\n",
        "  } else {", "\n",
        "    norm_test_result <- ad.test(res)", "\n",
        "    p_val <- norm_test_result$p.value", "\n",
        "    cat(paste0('Hasil Uji Normalitas Residual (Anderson-Darling Test):\\nNilai p (p-value): ', format(round(p_val, 4), scientific = FALSE), '.\\n'))", "\n",
        "    if (p_val < 0.05) {", "\n",
        "      cat('Karena nilai p < 0.05, kita menolak H0.\\nIni berarti ada bukti statistik yang kuat bahwa residual TIDAK berdistribusi normal.\\nPelanggaran asumsi normalitas bisa mengindikasikan bahwa model mungkin tidak optimal atau ada outlier.')", "\n",
        "    } else {", "\n",
        "      cat('Karena nilai p >= 0.05, kita gagal menolak H0.\\nIni berarti tidak ada cukup bukti statistik untuk menyimpulkan bahwa residual TIDAK berdistribusi normal (sering diinterpretasikan sebagai \\'residual berdistribusi normal\\').')", "\n",
        "    }", "\n",
        "  }", "\n",
        "}", "\n",
        "```\n\n",
        
        "#### 3. Asumsi Homoskedastisitas", "\n",
        "Uji yang Digunakan: Non-Constant Variance Test (ncvTest dari paket `car`). Asumsi homoskedastisitas mengasumsikan bahwa varians residual adalah konstan di semua level variabel independen.\n",
        "Hipotesis Nol (H0): Varians residual adalah konstan (Homoskedastisitas).\n",
        "Hipotesis Alternatif (H1): Varians residual tidak konstan (Heteroskedastisitas).\n",
        "Aturan Keputusan: Tolak H0 jika nilai p < tingkat signifikansi (misalnya 0.05).\n\n",
        "```{r homoscedasticity-test}", "\n",
        "dep_var <- '", input$reg_dependent_var, "'", "\n",
        "indep_vars <- c('", paste(input$reg_independent_vars, collapse = "', '"), "')", "\n",
        "model_vars_names <- c(dep_var, indep_vars)", "\n",
        "clean_reg_data <- reg_data_full %>% select(all_of(model_vars_names))", "\n",
        "for (iv in indep_vars) {", "\n",
        "  if (iv %in% all_categorical_vars_in_rmd && !is.factor(clean_reg_data[[iv]])) {", "\n",
        "    clean_reg_data[[iv]] <- as.factor(clean_reg_data[[iv]])", "\n",
        "  }", "\n",
        "}", "\n",
        "clean_reg_data <- na.omit(clean_reg_data)", "\n",
        "if (nrow(clean_reg_data) == 0) {", "\n",
        "  cat('Error: Data tidak cukup untuk uji homoskedastisitas.')", "\n",
        "} else {", "\n",
        "  formula_str <- paste0(dep_var, ' ~ ', paste(indep_vars, collapse = ' + '))", "\n",
        "  model <- lm(as.formula(formula_str), data = clean_reg_data)", "\n",
        "  ncvTest(model)", "\n",
        "}", "\n",
        "```", "\n",
        "**Interpretasi Homoskedastisitas:**\n",
        "```{r homoscedasticity-interpret, echo=FALSE}", "\n",
        "dep_var <- '", input$reg_dependent_var, "'", "\n",
        "indep_vars <- c('", paste(input$reg_independent_vars, collapse = "', '"), "')", "\n",
        "model_vars_names <- c(dep_var, indep_vars)", "\n",
        "clean_reg_data <- reg_data_full %>% select(all_of(model_vars_names))", "\n",
        "for (iv in indep_vars) {", "\n",
        "  if (iv %in% all_categorical_vars_in_rmd && !is.factor(clean_reg_data[[iv]])) {", "\n",
        "    clean_reg_data[[iv]] <- as.factor(clean_reg_data[[iv]])", "\n",
        "  }", "\n",
        "}", "\n",
        "clean_reg_data <- na.omit(clean_reg_data)", "\n",
        "if (nrow(clean_reg_data) == 0) {", "\n",
        "  cat('Interpretasi tidak tersedia karena data tidak cukup.')", "\n",
        "} else {", "\n",
        "  formula_str <- paste0(dep_var, ' ~ ', paste(indep_vars, collapse = ' + '))", "\n",
        "  model <- lm(as.formula(formula_str), data = clean_reg_data)", "\n",
        "  ncv_test_result <- ncvTest(model)", "\n",
        "  p_val <- ncv_test_result$p", "\n",
        "  cat(paste0('Hasil Uji Homoskedastisitas (Non-Constant Variance Test):\\nNilai p (p-value): ', format(round(p_val, 4), scientific = FALSE), '.\\n'))", "\n",
        "  if (p_val < 0.05) {", "\n",
        "    cat('Karena nilai p < 0.05, kita menolak H0.\\nIni berarti ada bukti statistik yang kuat bahwa varians residual TIDAK konstan (Heteroskedastisitas).\\nPelanggaran asumsi ini dapat menyebabkan estimasi standar error yang bias dan kesimpulan inferensial yang tidak akurat.')", "\n",
        "  } else {", "\n",
        "    cat('Karena nilai p >= 0.05, kita gagal menolak H0.\\nIni berarti tidak ada cukup bukti statistik untuk menyimpulkan bahwa varians residual TIDAK konstan (sering diinterpretasikan sebagai \\'varians residual adalah konstan / Homoskedastisitas\\').')", "\n",
        "  }", "\n",
        "}", "\n",
        "```\n\n",
        
        "#### 4. Asumsi Independensi Residual", "\n",
        "Uji yang Digunakan: Durbin-Watson Test (durbinWatsonTest dari paket `car`). Asumsi independensi residual mengasumsikan bahwa residual satu observasi tidak berkorelasi dengan residual observasi lain.\n",
        "Hipotesis Nol (H0): Residual independen (tidak ada autokorelasi).\n",
        "Hipotesis Alternatif (H1): Residual tidak independen (ada autokorelasi).\n",
        "Aturan Keputusan: Tolak H0 jika p-value < tingkat signifikansi (misalnya 0.05).\n\n",
        "```{r durbin-watson-test}", "\n",
        "dep_var <- '", input$reg_dependent_var, "'", "\n",
        "indep_vars <- c('", paste(input$reg_independent_vars, collapse = "', '"), "')", "\n",
        "model_vars_names <- c(dep_var, indep_vars)", "\n",
        "clean_reg_data <- reg_data_full %>% select(all_of(model_vars_names))", "\n",
        "for (iv in indep_vars) {", "\n",
        "  if (iv %in% all_categorical_vars_in_rmd && !is.factor(clean_reg_data[[iv]])) {", "\n",
        "    clean_reg_data[[iv]] <- as.factor(clean_reg_data[[iv]])", "\n",
        "  }", "\n",
        "}", "\n",
        "clean_reg_data <- na.omit(clean_reg_data)", "\n",
        "if (nrow(clean_reg_data) == 0) {", "\n",
        "  cat('Error: Data tidak cukup untuk uji Durbin-Watson.')", "\n",
        "} else {", "\n",
        "  formula_str <- paste0(dep_var, ' ~ ', paste(indep_vars, collapse = ' + '))", "\n",
        "  model <- lm(as.formula(formula_str), data = clean_reg_data)", "\n",
        "  durbinWatsonTest(model)", "\n",
        "}", "\n",
        "```", "\n",
        "**Interpretasi Independensi Residual:**\n",
        "```{r durbin-watson-interpret, echo=FALSE}", "\n",
        "dep_var <- '", input$reg_dependent_var, "'", "\n",
        "indep_vars <- c('", paste(input$reg_independent_vars, collapse = "', '"), "')", "\n",
        "model_vars_names <- c(dep_var, indep_vars)", "\n",
        "clean_reg_data <- reg_data_full %>% select(all_of(model_vars_names))", "\n",
        "for (iv in indep_vars) {", "\n",
        "  if (iv %in% all_categorical_vars_in_rmd && !is.factor(clean_reg_data[[iv]])) {", "\n",
        "    clean_reg_data[[iv]] <- as.factor(clean_reg_data[[iv]])", "\n",
        "  }", "\n",
        "}", "\n",
        "clean_reg_data <- na.omit(clean_reg_data)", "\n",
        "if (nrow(clean_reg_data) == 0) {", "\n",
        "  cat('Interpretasi tidak tersedia karena data tidak cukup.')", "\n",
        "} else {", "\n",
        "  formula_str <- paste0(dep_var, ' ~ ', paste(indep_vars, collapse = ' + '))", "\n",
        "  model <- lm(as.formula(formula_str), data = clean_reg_data)", "\n",
        "  dw_test_result <- durbinWatsonTest(model)", "\n",
        "  dw_value <- dw_test_result$dw[1]", "\n",
        "  dw_p_val <- dw_test_result$p[1]", "\n",
        "  cat(paste0('Hasil Uji Independensi Residual (Durbin-Watson Test):\\nNilai Durbin-Watson (d): ', round(dw_value, 4), '\\nNilai p (p-value): ', format(round(dw_p_val, 4), scientific = FALSE), '.\\n'))", "\n",
        "  if (dw_p_val < 0.05) {", "\n",
        "    cat('Karena nilai p < 0.05, kita menolak H0.\\nIni berarti ada bukti statistik yang signifikan bahwa residual TIDAK independen (ada autokorelasi).\\nAutokorelasi dapat terjadi pada data deret waktu atau data spasial dan dapat menyebabkan standar error yang bias.')", "\n",
        "  } else {", "\n",
        "    cat('Karena nilai p >= 0.05, kita gagal menolak H0.\\nIni berarti tidak ada cukup bukti statistik untuk menyimpulkan bahwa residual TIDAK independen (sering diinterpretasikan sebagai \\'residual adalah independen\\').\\nNilai d mendekati 2 juga mendukung asumsi independensi.')", "\n",
        "  }", "\n",
        "}", "\n",
        "```\n\n",
        
        "#### 5. Asumsi Tidak Ada Multikolinearitas", "\n",
        "Uji yang Digunakan: Variance Inflation Factor (VIF). Asumsi multikolinearitas mengasumsikan bahwa tidak ada korelasi tinggi antar variabel independen.\n",
        "Aturan Keputusan: Nilai VIF > 5 (atau kadang > 10) menunjukkan adanya multikolinearitas yang signifikan.\n\n",
        "```{r vif-test}", "\n",
        "dep_var <- '", input$reg_dependent_var, "'", "\n",
        "indep_vars <- c('", paste(input$reg_independent_vars, collapse = "', '"), "')", "\n",
        "model_vars_names <- c(dep_var, indep_vars)", "\n",
        "clean_reg_data <- reg_data_full %>% select(all_of(model_vars_names))", "\n",
        "for (iv in indep_vars) {", "\n",
        "  if (iv %in% all_categorical_vars_in_rmd && !is.factor(clean_reg_data[[iv]])) {", "\n",
        "    clean_reg_data[[iv]] <- as.factor(clean_reg_data[[iv]])", "\n",
        "  }", "\n",
        "}", "\n",
        "clean_reg_data <- na.omit(clean_reg_data)", "\n",
        "if (nrow(clean_reg_data) == 0) {", "\n",
        "  cat('Error: Data tidak cukup untuk uji multikolinearitas.')", "\n",
        "} else if (length(indep_vars) <= 1) {", "\n",
        "  cat('VIF hanya relevan jika ada dua atau lebih variabel independen.')", "\n",
        "} else {", "\n",
        "  formula_str <- paste0(dep_var, ' ~ ', paste(indep_vars, collapse = ' + '))", "\n",
        "  model <- lm(as.formula(formula_str), data = clean_reg_data)", "\n",
        "  vif(model)", "\n",
        "}", "\n",
        "```", "\n",
        "**Interpretasi Multikolinearitas:**\n",
        "```{r vif-interpret, echo=FALSE}", "\n",
        "dep_var <- '", input$reg_dependent_var, "'", "\n",
        "indep_vars <- c('", paste(input$reg_independent_vars, collapse = "', '"), "')", "\n",
        "model_vars_names <- c(dep_var, indep_vars)", "\n",
        "clean_reg_data <- reg_data_full %>% select(all_of(model_vars_names))", "\n",
        "for (iv in indep_vars) {", "\n",
        "  if (iv %in% all_categorical_vars_in_rmd && !is.factor(clean_reg_data[[iv]])) {", "\n",
        "    clean_reg_data[[iv]] <- as.factor(clean_reg_data[[iv]])", "\n",
        "  }", "\n",
        "}", "\n",
        "clean_reg_data <- na.omit(clean_reg_data)", "\n",
        "if (nrow(clean_reg_data) == 0) {", "\n",
        "  cat('Interpretasi tidak tersedia karena data tidak cukup.')", "\n",
        "} else if (length(indep_vars) <= 1) {", "\n",
        "  cat('VIF hanya relevan jika ada dua atau lebih variabel independen. Tidak ada multikolinearitas yang perlu diuji dengan hanya satu prediktor.')", "\n",
        "} else {", "\n",
        "  formula_str <- paste0(dep_var, ' ~ ', paste(indep_vars, collapse = ' + '))", "\n",
        "  model <- lm(as.formula(formula_str), data = clean_reg_data)", "\n",
        "  vif_results <- vif(model)", "\n",
        "  interpret_lines <- character(0)", "\n",
        "  has_high_vif <- FALSE", "\n",
        "  for (i in seq_along(vif_results)) {", "\n",
        "    var_name <- names(vif_results)[i]", "\n",
        "    vif_value <- vif_results[i]", "\n",
        "    if (vif_value >= 5) {", "\n",
        "      has_high_vif <- TRUE", "\n",
        "      interpret_lines <- c(interpret_lines,", "\n",
        "                           paste0('Variabel \\'', var_name, '\\' memiliki VIF = ', round(vif_value, 2), ' (tinggi).'))", "\n",
        "    } else {", "\n",
        "      interpret_lines <- c(interpret_lines,", "\n",
        "                           paste0('Variabel \\'', var_name, '\\' memiliki VIF = ', round(vif_value, 2), ' (aman).'))", "\n",
        "    }", "\n",
        "  }", "\n",
        "  if (has_high_vif) {", "\n",
        "    cat(paste(interpret_lines, collapse = '\\n'))", "\n",
        "    cat('\\nMultikolinearitas dapat membuat koefisien regresi tidak stabil dan sulit diinterpretasikan. Pertimbangkan untuk menghapus salah satu variabel yang sangat berkorelasi atau menggunakan teknik lain (misalnya PCA, regresi ridge).')", "\n",
        "  } else {", "\n",
        "    cat(paste(interpret_lines, collapse = '\\n'))", "\n",
        "    cat('\\nNilai VIF (Variance Inflation Factor) menunjukkan tidak ada multikolinearitas yang signifikan antar variabel independen yang dipilih (semua VIF < 5 atau < 10).')", "\n",
        "  }", "\n",
        "}", "\n",
        "```\n\n"
      )
      writeChar(rmd_content, file, eos = NULL)
    }
  )
  
}

# Jalankan Aplikasi Shiny
shinyApp(ui = ui, server = server)