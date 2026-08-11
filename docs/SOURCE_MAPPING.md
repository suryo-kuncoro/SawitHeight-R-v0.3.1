# Pemetaan Tutorial ke Aplikasi

| Tahap tutorial | Implementasi pada SawitHeight R |
|---|---|
| 00 Persiapan Environment R | Deteksi `Rscript.exe`, pemeriksaan versi package, dan instalasi ke user library. |
| 01 Load Dense Point Cloud | Form input LAS/LAZ, pembacaan header untuk validasi, kemudian `readLAS()` saat run. |
| 02 Quality Check | Penghapusan duplikat, SOR noise, raster densitas, spacing, dan estimasi ukuran CHM. |
| 03 Klasifikasi Ground | CSF dengan cloth resolution, threshold, rigidness, slope smoothing, dan `last_returns = FALSE`. |
| 03B Referensi Tanah Independen | Empat cabang: CSF→TIN; DTM independen; GCP bias correction; dan GCP ground anchor TIN. Mode GCP menghasilkan statistik residual dan output QC tersendiri. |
| 04 Normalisasi TIN | `normalize_height(..., tin())`, validasi ground Z, dan normalized LAZ opsional. |
| 05 Generate nCHM | `rasterize_canopy(..., pitfree())`, resolusi otomatis/manual, GeoTIFF dan QC PNG. |
| 06 Zonal Statistics | Buffer bermeter, masking threshold kanopi, fractional coverage, statistik tinggi dan QC support. |
| 07 Klasifikasi & Export | Kelas tinggi, CSV, GeoPackage, Shapefile opsional, JSON, manifest, dan laporan HTML. |

## Perubahan yang disengaja

1. Backend tidak menggunakan `tidyverse`; operasi atribut sederhana memakai base R untuk
   mengurangi ukuran dan jumlah dependensi runtime.
2. `last_returns = FALSE` digunakan pada CSF karena dense cloud fotogrametri bukan data
   laser return.
3. Resolusi otomatis diberi batas minimum untuk mencegah raster sangat besar pada point
   cloud ultra-dense.
4. GeoPackage dijadikan output utama karena Shapefile membatasi panjang nama field.
5. Nilai di luar raster, dukungan piksel rendah, dan tutupan kanopi rendah diberi flag QC,
   bukan disembunyikan.

## Detail Step 03B v0.2.0

- **DTM independen:** raster GeoTIFF satu band digunakan langsung untuk `normalize_height()`.
- **GCP bias:** ground CSF dirasterisasi menjadi DTM TIN, elevasi DTM diekstrak pada GCP, residual `Z_DTM - Z_GCP` dihitung, lalu mean bias dapat dikurangkan dari seluruh DTM. SD dilaporkan tetapi tidak dipakai sebagai keputusan otomatis karena tutorial tidak menetapkan threshold numerik.
- **GCP anchor:** ground hasil CSF digabung secara konseptual dengan XYZ GCP berkelas ground untuk membentuk permukaan TIN referensi. Implementasi aplikasi membentuk LAS referensi ground-only, merasterisasi TIN, lalu memakai raster tersebut untuk normalisasi sehingga header LAS utama tidak perlu dimodifikasi secara rapuh.
