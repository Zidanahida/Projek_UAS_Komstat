# Dashboard Analisis Data Kerentanan Sosial (SoVI) di Indonesia

Dashboard interaktif ini dikembangkan menggunakan R Shiny untuk memfasilitasi eksplorasi, manajemen, analisis statistik, dan pengujian asumsi data terkait Indeks Kerentanan Sosial (SoVI) di 511 kabupaten/kota di Indonesia. Proyek ini dibuat sebagai bagian dari tugas Ujian Akhir Semester (UAS) mata kuliah Komputasi Statistik.

## Fitur Utama

Dashboard ini menyediakan antarmuka yang ramah pengguna untuk melakukan berbagai analisis data multi-dimensi:

* **Beranda:** Menampilkan ringkasan statistik data, informasi umum tentang dashboard, fitur utama, dan deskripsi variabel data SoVI.
* **Manajemen Data:**
    * Melihat tabel lengkap dataset `sovi_data.csv`.
    * Melakukan transformasi data dengan mengkategorikan variabel kontinu menggunakan berbagai metode (Natural Breaks, Equal Interval, Quantile, dll.).
    * Menyimpan dan mengunduh variabel kategori yang telah dibuat.
    * Mengunduh dataset asli (`sovi_data.csv`).
* **Eksplorasi Data:**
    * Menampilkan statistik deskriptif untuk variabel numerik.
    * Membuat visualisasi dasar seperti Histogram, Box Plot, dan Scatter Plot.
    * Menampilkan peta tematik distrik di Indonesia berdasarkan variabel yang dipilih.
    * Mengunduh plot dalam format PNG.
    * Mengunduh laporan eksplorasi data dalam format RMarkdown (`.Rmd`) yang mencakup statistik, plot, dan interpretasi.
* **Uji Asumsi Data:**
    * Melakukan Uji Normalitas (Anderson-Darling Test).
    * Melakukan Uji Homogenitas Varians (Levene's Test).
    * Mengunduh laporan uji asumsi dalam format RMarkdown (`.Rmd`) yang berisi hasil dan interpretasi.
* **Statistik Inferensia:**
    * Melakukan Uji-t Satu Sampel.
    * Melakukan Uji-t Dua Sampel.
    * Melakukan Uji Proporsi (1 atau 2 kelompok).
    * Melakukan Uji Varians (F-Test) untuk 2 kelompok.
    * Melakukan Analisis Varians (ANOVA) One-Way dan Two-Way.
    * Mengunduh laporan statistik inferensia dalam format RMarkdown (`.Rmd`) yang berisi hasil dan interpretasi.
* **Regresi Linear Berganda:**
    * Mengonfigurasi dan menjalankan model regresi linear berganda.
    * Melihat output ringkasan model dan interpretasinya.
    * Melakukan dan menginterpretasikan uji asumsi regresi linear (Linearitas, Normalitas Residual, Homoskedastisitas, Independensi Residual, Multikolinearitas).
    * Mengunduh laporan analisis regresi dalam format RMarkdown (`.Rmd`) yang mencakup semua hasil dan interpretasi.

## Struktur Proyek
Repositori ini memiliki struktur folder sebagai berikut:
Projek_UAS_Komstat/Anda benar sekali! Mohon maaf, representasi struktur folder dengan karakter ── dalam satu baris seringkali tidak dirender dengan baik oleh GitHub Markdown karena dianggap sebagai teks biasa yang panjang, bukan sebagai hierarki visual.

Saya akan memperbaiki bagian "Struktur Proyek" di file README.md Anda agar dirender dengan rapi di GitHub. Cara terbaik adalah menempatkannya di dalam code block Markdown agar format indentasinya terjaga.

Berikut adalah bagian yang perlu Anda ganti di file README.md Anda:

Bagian yang perlu Anda Ganti (di README.md Anda):

Markdown

## Struktur Proyek

Repositori ini memiliki struktur folder sebagai berikut: Projek_UAS_Komstat/ ├── app.R ├── sovi_data.csv ├── indonesia_simplified.geojson └── ui/ ├── ui_beranda.R ├── ui_manajemen_data.R ├── ui_eksplorasi_data.R ├── ui_uji_asumsi_data.R ├── ui_statistik_inferensia.R └── ui_regresi_linear_berganda.R
Ganti dengan kode ini (di README.md Anda):

Markdown

## Struktur Proyek

Repositori ini memiliki struktur folder sebagai berikut:

- Projek_UAS_Komstat/
    - app.R
    - sovi_data.csv
    - indonesia_simplified.geojson
    - ui/
        - ui_beranda.R
        - ui_manajemen_data.R
        - ui_eksplorasi_data.R
        - ui_uji_asumsi_data.R
        - ui_statistik_inferensia.R
        - ui_regresi_linear_berganda.R

* `app.R`: File utama aplikasi Shiny yang berisi logika *server* dan menggabungkan komponen UI.
* `sovi_data.csv`: Dataset asli yang digunakan dalam analisis.
* `indonesia_simplified.geojson`: Data geografis yang digunakan untuk visualisasi peta.
* `ui/`: Direktori yang berisi definisi antarmuka pengguna (UI) untuk setiap tab/menu.

## Cara Menggunakan Dashboard
Untuk menjalankan dan menggunakan dashboard ini, ikuti langkah-langkah berikut:

### Prasyarat
Pastikan Anda memiliki hal-hal berikut terinstal di komputer Anda:
* [R](https://cran.r-project.org/) (versi 4.0 atau lebih tinggi direkomendasikan)
* [RStudio](https://www.rstudio.com/products/rstudio/download/) (direkomendasikan untuk pengembangan dan penggunaan yang mudah)
* Git (untuk mengkloning repositori)

### Instalasi dan Peluncuran
1.  **Kloning Repositori:**
    Buka terminal atau Git Bash di komputer Anda, lalu kloning repositori ini:
    ```bash
    git clone [https://github.com/Zidanahida/Projek_UAS_Komstat.git](https://github.com/Zidanahida/Projek_UAS_Komstat.git)
    ```

2.  **Buka Proyek di RStudio:**
    Buka RStudio, lalu pilih `File` > `Open Project...` dan arahkan ke folder `Projek_UAS_Komstat` yang baru saja Anda kloning.
3.  **Instal Paket R yang Diperlukan:**
    Buka `app.R` di RStudio. RStudio akan secara otomatis mendeteksi paket-paket yang dibutuhkan. Anda bisa menginstal semuanya dengan menjalankan perintah berikut di konsol RStudio:
    ```R
    install.packages(c("shiny", "dplyr", "ggplot2", "DT", "car", "nortest", "classInt", "rmarkdown", "sf", "leaflet", "bslib"))
    ```
    *Pastikan paket `bslib` Anda adalah versi terbaru (0.4.0 atau lebih tinggi) untuk fungsi kartu di Beranda. Jika masih ada error setelah instalasi, coba restart sesi R Anda (`Session > Restart R`) dan ulangi `install.packages("bslib")`.*
4.  **Jalankan Aplikasi:**
    Setelah semua paket terinstal, klik tombol **Run App** di pojok kanan atas jendela *script* `app.R` di RStudio. Atau, jalankan perintah berikut di konsol:
    ```R
    shiny::runApp()
    ```

### Mengunduh Laporan RMarkdown
Beberapa fitur di dashboard memungkinkan Anda mengunduh laporan dalam format RMarkdown (`.Rmd`). Agar *file* `.Rmd` tersebut dapat dijalankan dengan benar di komputer Anda (misalnya, di RStudio), perhatikan hal berikut:
* **File Data**: Pastikan Anda mengunduh **Dataset Asli (sovi_data.csv)** dan **Data Kategori (categorized_data.csv)** (jika Anda telah mengkategorikan dan menyimpannya) dari menu "Manajemen Data".
* **Lokasi File**: Letakkan *file* `.Rmd` yang Anda unduh bersama dengan `sovi_data.csv` dan `categorized_data.csv` (jika ada) **dalam satu folder yang sama** di komputer Anda.
* **Nama File**: Harap **tidak mengubah nama file** `sovi_data.csv` dan `categorized_data.csv` untuk memastikan *file* `.Rmd` dapat menemukan dan membacanya dengan benar.

Selamat menganalisis data Anda!

---
*Created by Muhamad Zidan Kurnia Ahida*
