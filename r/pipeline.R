options(warn = 1, stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1) args[[1]] else ''
config_path <- if (length(args) >= 2) args[[2]] else ''

base_event <- function(type, message, level = 'error') {
  fields <- c(
    paste0('\"type\":', encodeString(as.character(type), quote = '"')),
    paste0('\"level\":', encodeString(as.character(level), quote = '"')),
    paste0('\"message\":', encodeString(as.character(message), quote = '"'))
  )
  cat('APP_EVENT:{', paste(fields, collapse = ','), '}\n', sep = '')
  flush.console()
}

if (!mode %in% c('validate', 'run') || !nzchar(config_path) || !file.exists(config_path)) {
  base_event('fatal', 'Argumen backend tidak valid atau file konfigurasi tidak ditemukan.')
  quit(status = 2)
}

if (!requireNamespace('jsonlite', quietly = TRUE)) {
  base_event('fatal', 'Package jsonlite belum terpasang. Jalankan pemeriksaan environment dan instal package.')
  quit(status = 3)
}

required_packages <- c('lidR', 'terra', 'sf', 'exactextractr', 'RCSF')
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  base_event('fatal', paste('Package R belum tersedia:', paste(missing_packages, collapse = ', ')))
  quit(status = 4)
}

suppressPackageStartupMessages({
  library(lidR)
  library(terra)
  library(sf)
  library(exactextractr)
})

options(lidR.raster.default = 'terra')

cfg <- jsonlite::fromJSON(config_path, simplifyVector = TRUE)
inputs <- cfg$inputs
p <- cfg$parameters
run_dir <- normalizePath(cfg$app$run_dir, winslash = '/', mustWork = FALSE)
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
log_path <- file.path(run_dir, if (mode == 'run') 'analysis.log' else 'validation.log')
current_stage <- 'initialization'

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x) || identical(x, '')) y else x

emit <- function(type, ..., .level = NULL) {
  payload <- list(type = type, timestamp = format(Sys.time(), '%Y-%m-%dT%H:%M:%S%z'))
  dots <- list(...)
  payload <- c(payload, dots)
  if (!is.null(.level)) payload$level <- .level
  text <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = 'null', na = 'null', digits = 10)
  cat('APP_EVENT:', text, '\n', sep = '')
  flush.console()
}

write_log <- function(level, message) {
  line <- sprintf('[%s] [%s] [%s] %s', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), level, current_stage, message)
  cat(line, '\n', file = log_path, append = TRUE)
  emit('log', level = tolower(level), stage = current_stage, message = message)
}

set_stage <- function(id, label, progress) {
  current_stage <<- id
  emit('progress', stage = id, label = label, progress = progress)
  cat(sprintf('[%s] [STAGE] [%s] %s\n', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), id, label),
      file = log_path, append = TRUE)
}

stop_app <- function(message) stop(message, call. = FALSE)

as_num <- function(x, default = NA_real_) {
  value <- suppressWarnings(as.numeric(x))
  if (!length(value) || is.na(value)) default else value
}

as_int <- function(x, default = NA_integer_) {
  value <- suppressWarnings(as.integer(x))
  if (!length(value) || is.na(value)) default else value
}

as_bool <- function(x, default = FALSE) {
  if (is.null(x) || !length(x) || is.na(x)) default else isTRUE(x)
}

ensure_writable_dir <- function(path_value) {
  dir.create(path_value, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path_value)) stop_app(paste('Folder output tidak dapat dibuat:', path_value))
  test_file <- file.path(path_value, paste0('.write_test_', Sys.getpid()))
  ok <- tryCatch({
    writeLines('test', test_file)
    unlink(test_file)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) stop_app(paste('Folder output tidak dapat ditulis:', path_value))
}

check_extension <- function(path_value, allowed, label) {
  ext <- tolower(tools::file_ext(path_value))
  if (!ext %in% allowed) stop_app(paste(label, 'harus berformat', paste(paste0('.', allowed), collapse = ', ')))
}

get_las_crs <- function(obj, fallback_epsg = 0L) {
  crs_obj <- suppressWarnings(sf::st_crs(obj))
  if (is.na(crs_obj) && fallback_epsg > 0L) {
    crs_obj <- sf::st_crs(fallback_epsg)
    try(suppressWarnings(sf::st_crs(obj) <- crs_obj), silent = TRUE)
  }
  list(object = obj, crs = crs_obj)
}

validate_vector_points <- function(points, label = 'Data titik') {
  if (!inherits(points, 'sf')) stop_app(paste(label, 'gagal dibaca sebagai objek sf.'))
  if (!nrow(points)) stop_app(paste(label, 'tidak memiliki fitur.'))
  geom_types <- unique(as.character(sf::st_geometry_type(points, by_geometry = TRUE)))
  if (!all(geom_types %in% c('POINT', 'MULTIPOINT'))) {
    stop_app(paste('Geometri', label, 'harus POINT/MULTIPOINT, ditemukan:', paste(geom_types, collapse = ', ')))
  }
  if (any(geom_types == 'MULTIPOINT')) points <- suppressWarnings(sf::st_cast(points, 'POINT'))
  if (any(sf::st_is_empty(points))) {
    write_log('WARNING', paste('Terdapat geometri kosong pada', label, '; fitur kosong akan dihapus.'))
    points <- points[!sf::st_is_empty(points), , drop = FALSE]
  }
  if (!nrow(points)) stop_app(paste('Tidak ada', label, 'valid setelah geometri kosong dihapus.'))
  points
}

crs_equal <- function(a, b) {
  if (is.na(a) || is.na(b)) return(FALSE)
  isTRUE(a == b)
}

extent_contains_bbox <- function(raster_obj, bbox_obj, tolerance = 0) {
  as.numeric(terra::xmin(raster_obj)) <= as.numeric(bbox_obj['xmin']) + tolerance &&
    as.numeric(terra::xmax(raster_obj)) >= as.numeric(bbox_obj['xmax']) - tolerance &&
    as.numeric(terra::ymin(raster_obj)) <= as.numeric(bbox_obj['ymin']) + tolerance &&
    as.numeric(terra::ymax(raster_obj)) >= as.numeric(bbox_obj['ymax']) - tolerance
}

get_ground_mode <- function() {
  mode_value <- as.character(p$ground_reference_mode %||% '')
  if (!nzchar(mode_value)) mode_value <- if (as_bool(p$use_external_dtm, FALSE)) 'external_dtm' else 'csf_tin'
  valid <- c('csf_tin', 'external_dtm', 'gcp_bias', 'gcp_anchor')
  if (!mode_value %in% valid) stop_app(paste('Mode referensi tanah tidak dikenali:', mode_value))
  mode_value
}

ground_mode_label <- function(mode_value) {
  labels <- c(
    csf_tin = 'CSF dense cloud → TIN',
    external_dtm = 'DTM independen',
    gcp_bias = 'GCP — koreksi bias DTM',
    gcp_anchor = 'GCP — ground anchor TIN'
  )
  unname(labels[[mode_value]] %||% mode_value)
}

normalize_period_code <- function(x, label = 'Kode periode') {
  value <- toupper(trimws(as.character(x %||% '')))
  if (!grepl('^[A-Z0-9]{1,3}$', value)) {
    stop_app(paste(label, 'harus 1-3 karakter huruf/angka, contoh D1, D2, P01.'))
  }
  value
}

period_field <- function(prefix, period_code) {
  nm <- paste0(prefix, '_', period_code)
  if (nchar(nm) > 10) stop_app(paste('Nama field Shapefile melebihi 10 karakter:', nm))
  nm
}

is_monitoring_field <- function(x) {
  grepl('^(rerata|maks|min|sd|npix|cover|kelas|qc|tumb)_[A-Z0-9]{1,3}$', x)
}

write_ground_reference_summary <- function(summary_data, ground_dir) {
  dir.create(ground_dir, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(ground_dir, 'ground_reference_summary.json')
  jsonlite::write_json(summary_data, out, pretty = TRUE, auto_unbox = TRUE, na = 'null')
  out
}

safe_png <- function(filename, width = 1400, height = 900, expr) {
  grDevices::png(filename, width = width, height = height, res = 130)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
}

try_qc_png <- function(filename, label, expr) {
  tryCatch(
    safe_png(filename, expr = expr),
    error = function(e) {
      write_log('WARNING', paste('Gagal membuat', label, ':', conditionMessage(e)))
      if (file.exists(filename)) unlink(filename)
      invisible(FALSE)
    }
  )
}

make_html_report <- function(summary, preview, output_path) {
  esc <- function(x) {
    x <- gsub('&', '&amp;', as.character(x), fixed = TRUE)
    x <- gsub('<', '&lt;', x, fixed = TRUE)
    x <- gsub('>', '&gt;', x, fixed = TRUE)
    x
  }
  cards <- paste0(
    '<div class="card"><b>Jumlah titik input</b><span>', format(summary$input_points, big.mark = '.'), '</span></div>',
    '<div class="card"><b>Densitas rata-rata</b><span>', round(summary$avg_density_points_m2, 2), ' titik/m²</span></div>',
    '<div class="card"><b>Resolusi CHM</b><span>', summary$chm_resolution_m, ' m</span></div>',
    '<div class="card"><b>Referensi tanah</b><span>', esc(summary$ground_reference_label), '</span></div>',
    '<div class="card"><b>Periode</b><span>', esc(summary$period_code), '</span></div>',
    '<div class="card"><b>Mode</b><span>', esc(summary$monitoring_mode), '</span></div>',
    '<div class="card"><b>Titik pokok</b><span>', summary$tree_count, '</span></div>',
    '<div class="card"><b>Hasil valid</b><span>', summary$valid_height_count, '</span></div>',
    '<div class="card"><b>Pertumbuhan valid</b><span>', summary$growth_valid_count, '</span></div>',
    '<div class="card"><b>Ambang NORMAL</b><span>', ifelse(summary$monitoring_mode == 'monitoring', paste0(summary$growth_normal_min_m, ' m'), '-'), '</span></div>',
    '<div class="card"><b>Run</b><span>', esc(summary$run_name), '</span></div>'
  )
  rows <- apply(preview, 1, function(row) {
    paste0('<tr>', paste0('<td>', esc(row), '</td>', collapse = ''), '</tr>')
  })
  table_html <- paste0('<table><thead><tr>', paste0('<th>', esc(names(preview)), '</th>', collapse = ''),
                       '</tr></thead><tbody>', paste(rows, collapse = ''), '</tbody></table>')
  html <- paste0('<!doctype html><html><head><meta charset="utf-8"><title>SawitHeight R Report</title>',
    '<style>body{font-family:Arial,sans-serif;background:#0b0f11;color:#e7edee;padding:32px}',
    'h1{margin-bottom:6px}.sub{color:#91a1a5}.grid{display:grid;grid-template-columns:repeat(3,1fr);gap:12px;margin:24px 0}',
    '.card{background:#151d20;border:1px solid #2b393d;padding:16px;border-radius:10px;display:flex;flex-direction:column;gap:8px}',
    '.card b{color:#91a1a5;font-size:12px}.card span{font-size:20px;color:#e3a73f}',
    'table{border-collapse:collapse;width:100%;background:#12181b}th,td{border:1px solid #2b393d;padding:8px;font-size:12px;text-align:left}',
    'th{background:#1a2326;color:#e3a73f}</style></head><body>',
    '<h1>SawitHeight R — Ringkasan Analisis</h1><div class="sub">Estimasi tinggi pohon sawit dari dense point cloud fotogrametri</div>',
    '<div class="grid">', cards, '</div><h2>Pratinjau Hasil</h2>', table_html,
    '<p class="sub">Lihat run_config.json dan analysis.log untuk audit parameter dan proses.</p></body></html>')
  writeLines(html, output_path, useBytes = TRUE)
}

validate_common <- function(load_full_las = FALSE) {
  set_stage('validation', 'Memvalidasi file, CRS, dan parameter', 5)
  point_cloud <- normalizePath(inputs$point_cloud, winslash = '/', mustWork = TRUE)
  tree_points_path <- normalizePath(inputs$tree_points, winslash = '/', mustWork = TRUE)
  output_root <- normalizePath(inputs$output_root, winslash = '/', mustWork = FALSE)
  check_extension(point_cloud, c('las', 'laz'), 'Point cloud')
  check_extension(tree_points_path, c('shp', 'gpkg', 'geojson', 'json'), 'Titik pokok')
  ensure_writable_dir(output_root)

  ground_mode <- get_ground_mode()
  fallback_epsg <- as_int(p$fallback_epsg, 0L)
  header <- lidR::readLASheader(point_cloud)
  if (is.null(header)) stop_app('Header LAS/LAZ tidak dapat dibaca.')
  las_crs_info <- get_las_crs(header, fallback_epsg)
  header <- las_crs_info$object
  las_crs <- las_crs_info$crs
  if (is.na(las_crs)) stop_app('CRS point cloud kosong. Isi EPSG fallback yang sesuai.')
  if (isTRUE(sf::st_is_longlat(las_crs))) stop_app('CRS point cloud masih geografis/derajat. Gunakan CRS proyeksi bermeter untuk buffer dan tinggi.')
  crs_units <- tolower(as.character(las_crs$units_gdal %||% ''))
  if (nzchar(crs_units) && !crs_units %in% c('metre', 'meter', 'metres', 'meters', 'm')) {
    stop_app(paste('Satuan horizontal CRS bukan meter:', crs_units, '. Parameter buffer, CSF, dan resolusi aplikasi menggunakan meter.'))
  }
  if (!nzchar(crs_units)) write_log('WARNING', 'Satuan horizontal CRS tidak teridentifikasi; aplikasi mengasumsikan meter.')

  points <- suppressWarnings(sf::st_read(tree_points_path, quiet = TRUE))
  points <- validate_vector_points(points, 'titik pokok')
  if (is.na(sf::st_crs(points))) {
    write_log('WARNING', 'CRS titik pokok kosong; CRS point cloud diterapkan sebagai asumsi.')
    sf::st_crs(points) <- las_crs
  }
  points <- sf::st_transform(points, las_crs)
  requested_id <- as.character(p$tree_id_field %||% '')
  if (nzchar(requested_id) && !requested_id %in% names(points)) {
    stop_app(paste('Field ID pohon tidak ditemukan pada data titik:', requested_id))
  }

  monitoring_mode <- as.character(p$monitoring_mode %||% 'first')
  if (!monitoring_mode %in% c('first', 'monitoring')) stop_app('Mode periode harus first atau monitoring.')
  if (monitoring_mode == 'first' && !nzchar(requested_id)) {
    write_log('WARNING', 'Field ID pohon kosong pada baseline. Aplikasi membuat ID urut; untuk monitoring berulang sebaiknya gunakan ID pokok permanen yang stabil antarperiode.')
  }
  period_code <- normalize_period_code(p$period_code %||% 'D1', 'Kode periode saat ini')
  previous_period_code <- ''
  previous_points <- NULL
  previous_id_field <- ''
  matched_previous_count <- 0L

  if (monitoring_mode == 'monitoring') {
    if (!nzchar(requested_id)) stop_app('Monitoring membutuhkan Field ID pohon yang stabil dan unik.')
    if (anyDuplicated(as.character(points[[requested_id]]))) stop_app(paste('Field ID pohon tidak unik untuk monitoring:', requested_id))

    previous_period_code <- normalize_period_code(p$previous_period_code, 'Kode periode sebelumnya')
    if (identical(period_code, previous_period_code)) stop_app('Kode periode saat ini harus berbeda dari periode sebelumnya.')

    previous_path <- normalizePath(inputs$previous_result_shp, winslash = '/', mustWork = TRUE)
    check_extension(previous_path, c('shp'), 'Hasil periode sebelumnya')
    previous_points <- suppressWarnings(sf::st_read(previous_path, quiet = TRUE))
    previous_points <- validate_vector_points(previous_points, 'hasil periode sebelumnya')
    if (is.na(sf::st_crs(previous_points))) stop_app('CRS Shapefile periode sebelumnya kosong.')
    previous_points <- sf::st_transform(previous_points, las_crs)

    previous_id_field <- as.character(p$previous_tree_id_field %||% 'tree_id')
    if (!nzchar(previous_id_field)) previous_id_field <- 'tree_id'
    if (!previous_id_field %in% names(previous_points)) {
      stop_app(paste('Field ID pada Shapefile periode sebelumnya tidak ditemukan:', previous_id_field))
    }
    if (anyDuplicated(as.character(previous_points[[previous_id_field]]))) {
      stop_app(paste('Field ID periode sebelumnya tidak unik:', previous_id_field))
    }

    previous_max_field <- period_field('maks', previous_period_code)
    if (!previous_max_field %in% names(previous_points)) {
      stop_app(paste('Field tinggi maksimum periode sebelumnya tidak ditemukan:', previous_max_field))
    }
    current_max_field <- period_field('maks', period_code)
    if (current_max_field %in% names(previous_points)) {
      stop_app(paste('Periode', period_code, 'sudah ada pada Shapefile sebelumnya. Gunakan kode periode baru.'))
    }

    current_ids <- as.character(points[[requested_id]])
    previous_ids <- as.character(previous_points[[previous_id_field]])
    matched_previous_count <- sum(current_ids %in% previous_ids)
    if (matched_previous_count == 0) stop_app('Tidak ada ID pokok yang cocok antara data saat ini dan hasil periode sebelumnya.')
    if (matched_previous_count < length(current_ids)) {
      write_log('WARNING', paste(length(current_ids) - matched_previous_count, 'pokok tidak ditemukan pada periode sebelumnya; pertumbuhan akan NA.'))
    }
  }

  las_bbox <- sf::st_bbox(header)
  points_bbox <- sf::st_bbox(points)
  intersects_bbox <- !(points_bbox['xmax'] < las_bbox['xmin'] || points_bbox['xmin'] > las_bbox['xmax'] ||
                       points_bbox['ymax'] < las_bbox['ymin'] || points_bbox['ymin'] > las_bbox['ymax'])
  if (!intersects_bbox) stop_app('Extent titik pokok tidak beririsan dengan extent point cloud.')

  dtm <- NULL
  if (ground_mode == 'external_dtm') {
    dtm_path <- normalizePath(inputs$external_dtm, winslash = '/', mustWork = TRUE)
    check_extension(dtm_path, c('tif', 'tiff'), 'DTM independen')
    dtm <- terra::rast(dtm_path)
    if (terra::nlyr(dtm) != 1) stop_app('DTM independen harus mempunyai tepat satu band elevasi.')
    dtm_crs <- sf::st_crs(terra::crs(dtm, proj = TRUE))
    if (is.na(dtm_crs)) stop_app('CRS DTM independen kosong.')
    if (!crs_equal(dtm_crs, las_crs)) stop_app('CRS DTM independen berbeda dari CRS point cloud. Reproject DTM terlebih dahulu agar resolusi dan grid tetap terkontrol.')
    if (!extent_contains_bbox(dtm, las_bbox)) stop_app('DTM independen tidak menutupi seluruh extent point cloud.')
  }

  gcp <- NULL
  gcp_elevation_field <- ''
  gcp_id_field <- ''
  if (ground_mode %in% c('gcp_bias', 'gcp_anchor')) {
    gcp_path <- normalizePath(inputs$gcp_points, winslash = '/', mustWork = TRUE)
    check_extension(gcp_path, c('shp', 'gpkg', 'geojson', 'json'), 'Titik GCP')
    gcp <- suppressWarnings(sf::st_read(gcp_path, quiet = TRUE))
    gcp <- validate_vector_points(gcp, 'titik GCP')
    if (is.na(sf::st_crs(gcp))) stop_app('CRS titik GCP kosong. Tetapkan CRS yang benar sebelum menggunakan GCP sebagai referensi elevasi.')
    gcp <- sf::st_transform(gcp, las_crs)
    gcp_elevation_field <- as.character(p$gcp_elevation_field %||% '')
    if (!nzchar(gcp_elevation_field)) stop_app('Field elevasi tanah GCP belum diisi.')
    if (!gcp_elevation_field %in% names(gcp)) stop_app(paste('Field elevasi GCP tidak ditemukan:', gcp_elevation_field))
    z_ground <- suppressWarnings(as.numeric(gcp[[gcp_elevation_field]]))
    if (all(!is.finite(z_ground))) stop_app(paste('Field elevasi GCP tidak mengandung nilai numerik valid:', gcp_elevation_field))
    if (any(!is.finite(z_ground))) {
      write_log('WARNING', paste('GCP dengan elevasi kosong/non-numerik akan dibuang:', sum(!is.finite(z_ground)), 'fitur.'))
      gcp <- gcp[is.finite(z_ground), , drop = FALSE]
      z_ground <- z_ground[is.finite(z_ground)]
    }
    gcp[[gcp_elevation_field]] <- z_ground
    if (!nrow(gcp)) stop_app('Tidak ada GCP dengan elevasi valid.')

    gcp_id_field <- as.character(p$gcp_id_field %||% '')
    if (nzchar(gcp_id_field) && !gcp_id_field %in% names(gcp)) stop_app(paste('Field ID GCP tidak ditemukan:', gcp_id_field))
    if (!nzchar(gcp_id_field)) {
      gcp_id_field <- 'gcp_id'
      while (gcp_id_field %in% names(gcp)) gcp_id_field <- paste0(gcp_id_field, '_x')
      gcp[[gcp_id_field]] <- sprintf('GCP_%04d', seq_len(nrow(gcp)))
    }
    if (anyDuplicated(gcp[[gcp_id_field]])) write_log('WARNING', paste('Field ID GCP tidak unik:', gcp_id_field))

    gcp_bbox <- sf::st_bbox(gcp)
    gcp_intersects <- !(gcp_bbox['xmax'] < las_bbox['xmin'] || gcp_bbox['xmin'] > las_bbox['xmax'] ||
                        gcp_bbox['ymax'] < las_bbox['ymin'] || gcp_bbox['ymin'] > las_bbox['ymax'])
    if (!gcp_intersects) stop_app('Extent GCP tidak beririsan dengan extent point cloud.')
  }

  parameter_checks <- c(
    buffer_m = as_num(p$buffer_m),
    canopy_threshold_m = as_num(p$canopy_threshold_m),
    sor_k = as_num(p$sor_k),
    sor_m = as_num(p$sor_m),
    csf_cloth_resolution = as_num(p$csf_cloth_resolution),
    csf_class_threshold = as_num(p$csf_class_threshold),
    chm_resolution = as_num(p$chm_resolution),
    chm_min_auto_resolution = as_num(p$chm_min_auto_resolution),
    threads = as_num(p$threads)
  )
  if (ground_mode %in% c('gcp_bias', 'gcp_anchor')) {
    parameter_checks <- c(parameter_checks, ground_dtm_resolution_m = as_num(p$ground_dtm_resolution_m))
  }
  invalid <- names(parameter_checks)[!is.finite(parameter_checks) | parameter_checks <= 0]
  if (length(invalid)) stop_app(paste('Parameter harus lebih besar dari 0:', paste(invalid, collapse = ', ')))
  if (as_num(p$height_break_1_m) < 0 || as_num(p$height_break_2_m) < 0) stop_app('Batas kelas tinggi tidak boleh negatif.')
  if (as_num(p$height_break_1_m) >= as_num(p$height_break_2_m)) stop_app('Batas kelas tinggi pertama harus lebih kecil dari batas kedua.')
  if (!is.finite(as_num(p$growth_normal_min_m, 0.10)) || as_num(p$growth_normal_min_m, 0.10) <= 0) stop_app('Delta minimum status NORMAL harus lebih besar dari 0 meter.')
  rigidness <- as_int(p$csf_rigidness)
  if (!rigidness %in% 1:3) stop_app('CSF rigidness hanya boleh 1, 2, atau 3.')

  emit('validation-result', status = 'valid', point_cloud = point_cloud,
       tree_count = nrow(points), gcp_count = if (is.null(gcp)) 0 else nrow(gcp),
       crs = las_crs$input %||% las_crs$wkt,
       ground_reference_mode = ground_mode,
       ground_reference_label = ground_mode_label(ground_mode),
       monitoring_mode = monitoring_mode, period_code = period_code,
       previous_period_code = previous_period_code, matched_previous_count = matched_previous_count)

  list(point_cloud = point_cloud, points_path = tree_points_path, output_root = output_root,
       header = header, las_crs = las_crs, points = points, dtm = dtm, gcp = gcp,
       gcp_elevation_field = gcp_elevation_field, gcp_id_field = gcp_id_field,
       ground_mode = ground_mode, monitoring_mode = monitoring_mode, period_code = period_code,
       previous_period_code = previous_period_code, previous_points = previous_points,
       previous_id_field = previous_id_field, matched_previous_count = matched_previous_count)
}

run_pipeline <- function() {
  validated <- validate_common(TRUE)
  lidR::set_lidr_threads(as_int(p$threads, 1L))
  write_log('INFO', paste('R:', R.version.string))
  write_log('INFO', paste('lidR:', as.character(utils::packageVersion('lidR'))))
  write_log('INFO', paste('Point cloud:', validated$point_cloud))

  set_stage('load', 'Memuat dense point cloud', 10)
  las <- lidR::readLAS(validated$point_cloud)
  if (is.null(las) || lidR::is.empty(las)) stop_app('Point cloud kosong atau gagal dimuat.')
  las_crs_info <- get_las_crs(las, as_int(p$fallback_epsg, 0L))
  las <- las_crs_info$object
  input_points <- lidR::npoints(las)
  write_log('INFO', paste('Jumlah titik input:', format(input_points, big.mark = ',')))

  set_stage('clean', 'Membersihkan duplikat dan noise', 20)
  before_clean <- lidR::npoints(las)
  if (as_bool(p$remove_duplicates, TRUE)) {
    las <- lidR::filter_duplicates(las)
    write_log('INFO', paste('Duplikat XYZ dihapus:', before_clean - lidR::npoints(las)))
  }
  if (as_bool(p$remove_noise, TRUE)) {
    las <- lidR::classify_noise(las, lidR::sor(k = as_int(p$sor_k, 10L), m = as_num(p$sor_m, 3)))
    # classify_noise() hanya memberi label noise pada Classification = 18.
    # Gunakan filter_poi() untuk membuang titik berkelas noise agar kompatibel
    # dengan lidR lama maupun baru (remove_noise tidak diekspor pada beberapa versi).
    noise_count <- sum(las$Classification == 18L, na.rm = TRUE)
    las <- lidR::filter_poi(las, Classification != 18L)
    write_log('INFO', paste('Noise SOR dihapus:', noise_count))
  }
  if (lidR::is.empty(las)) stop_app('Semua titik terhapus pada tahap pembersihan.')

  set_stage('density', 'Menghitung densitas dan resolusi CHM', 28)
  density_raster <- lidR::rasterize_density(las, res = 1)
  avg_density <- as.numeric(terra::global(density_raster, 'mean', na.rm = TRUE)[1, 1])
  if (!is.finite(avg_density) || avg_density <= 0) stop_app('Densitas point cloud tidak dapat dihitung.')
  avg_spacing <- 1 / sqrt(avg_density)
  auto_res <- max(as_num(p$chm_min_auto_resolution, 0.05), round(avg_spacing * 2, 2))
  chm_res <- if (as_bool(p$chm_auto_resolution, TRUE)) auto_res else as_num(p$chm_resolution, 0.1)
  las_bbox_for_grid <- sf::st_bbox(las)
  estimated_cells <- ceiling((as.numeric(las_bbox_for_grid['xmax']) - as.numeric(las_bbox_for_grid['xmin'])) / chm_res) *
    ceiling((as.numeric(las_bbox_for_grid['ymax']) - as.numeric(las_bbox_for_grid['ymin'])) / chm_res)
  if (estimated_cells > 250000000) {
    stop_app(sprintf('Estimasi raster CHM mencapai %.1f juta piksel. Naikkan resolusi CHM atau potong point cloud agar risiko kehabisan RAM berkurang.', estimated_cells / 1e6))
  }
  if (estimated_cells > 100000000) {
    write_log('WARNING', sprintf('Estimasi raster CHM besar: %.1f juta piksel.', estimated_cells / 1e6))
  }
  write_log('INFO', sprintf('Densitas rata-rata %.3f titik/m2; spacing %.4f m; resolusi CHM %.3f m; estimasi %.1f juta piksel', avg_density, avg_spacing, chm_res, estimated_cells / 1e6))
  density_png <- file.path(run_dir, 'qc_density.png')
  try_qc_png(density_png, 'grafik densitas', terra::plot(density_raster, main = 'Densitas Point Cloud (titik/m2)'))

  ground_mode <- validated$ground_mode
  ground_label <- ground_mode_label(ground_mode)
  set_stage('ground', paste('Menyiapkan referensi tanah:', ground_label), 38)
  ground_pct <- NA_real_
  ground_reference_files <- character(0)
  ground_reference_summary <- list(
    mode = ground_mode,
    label = ground_label,
    gcp_count = if (is.null(validated$gcp)) 0L else nrow(validated$gcp)
  )
  normalization_surface <- NULL

  classify_ground_csf <- function(las_obj) {
    algorithm <- lidR::csf(
      sloop_smooth = as_bool(p$csf_slope_smooth, FALSE),
      class_threshold = as_num(p$csf_class_threshold, 0.3),
      cloth_resolution = as_num(p$csf_cloth_resolution, 0.5),
      rigidness = as_int(p$csf_rigidness, 2L)
    )
    classified <- lidR::classify_ground(las_obj, algorithm, last_returns = FALSE)
    ground_count_local <- sum(classified$Classification == lidR::LASGROUND, na.rm = TRUE)
    if (ground_count_local < 3) stop_app('Ground point kurang dari 3; referensi tanah TIN tidak dapat dibentuk. Gunakan DTM independen atau ubah parameter CSF.')
    pct <- ground_count_local / lidR::npoints(classified) * 100
    write_log('INFO', sprintf('Ground CSF: %s titik (%.2f%%)', format(ground_count_local, big.mark = ','), pct))
    if (pct < 5) write_log('WARNING', 'Persentase ground di bawah 5%. Pertimbangkan referensi tanah independen dari Step 03B.')
    list(las = classified, count = ground_count_local, pct = pct)
  }

  ground_dir <- file.path(run_dir, 'ground_reference')
  dir.create(ground_dir, recursive = TRUE, showWarnings = FALSE)

  if (ground_mode == 'external_dtm') {
    normalization_surface <- validated$dtm
    write_log('INFO', 'Normalisasi menggunakan DTM independen.')
    ground_reference_summary$source <- normalizePath(inputs$external_dtm, winslash = '/', mustWork = FALSE)
  } else {
    csf_result <- classify_ground_csf(las)
    las <- csf_result$las
    ground_pct <- csf_result$pct
    ground_reference_summary$ground_count <- csf_result$count
    ground_reference_summary$ground_pct <- ground_pct

    if (ground_mode == 'csf_tin') {
      write_log('INFO', 'Normalisasi akan menggunakan TIN langsung dari ground hasil CSF.')
    }

    if (ground_mode == 'gcp_bias') {
      ground_res <- as_num(p$ground_dtm_resolution_m, 0.5)
      dtm_raw <- lidR::rasterize_terrain(las, res = ground_res, algorithm = lidR::tin())
      if (!inherits(dtm_raw, 'SpatRaster')) dtm_raw <- terra::rast(dtm_raw)
      dtm_raw_path <- file.path(ground_dir, 'dtm_csf_raw.tif')
      terra::writeRaster(dtm_raw, dtm_raw_path, overwrite = TRUE, wopt = list(gdal = c('COMPRESS=LZW', 'TILED=YES')))
      ground_reference_files <- c(ground_reference_files, dtm_raw_path)

      gcp <- validated$gcp
      z_dtm <- as.numeric(terra::extract(dtm_raw, terra::vect(gcp), ID = FALSE)[, 1])
      z_field <- as.numeric(gcp[[validated$gcp_elevation_field]])
      valid_gcp <- is.finite(z_dtm) & is.finite(z_field)
      if (!any(valid_gcp)) stop_app('Tidak ada GCP yang memiliki nilai DTM valid untuk menghitung bias.')
      if (sum(!valid_gcp) > 0) write_log('WARNING', paste(sum(!valid_gcp), 'GCP tidak memiliki nilai DTM dan dikeluarkan dari statistik bias.'))
      residual <- z_dtm[valid_gcp] - z_field[valid_gcp]
      mean_bias <- mean(residual)
      sd_bias <- if (length(residual) > 1) stats::sd(residual) else NA_real_
      rmse_bias <- sqrt(mean(residual^2))
      write_log('INFO', sprintf('GCP bias: n=%d; mean=%+.4f m; SD=%s; RMSE=%.4f m', length(residual), mean_bias,
                                ifelse(is.finite(sd_bias), sprintf('%.4f m', sd_bias), 'NA'), rmse_bias))
      if (length(residual) < 3) write_log('WARNING', 'Jumlah GCP valid kurang dari 3; statistik bias sangat terbatas.')

      gcp_qc <- gcp[valid_gcp, , drop = FALSE]
      gcp_qc$z_dtm <- z_dtm[valid_gcp]
      gcp_qc$z_ground <- z_field[valid_gcp]
      gcp_qc$resid <- residual
      gcp_csv <- file.path(ground_dir, 'gcp_validation.csv')
      utils::write.csv(sf::st_drop_geometry(gcp_qc), gcp_csv, row.names = FALSE, na = '')
      gcp_shp <- file.path(ground_dir, 'gcp_residuals.shp')
      suppressWarnings(sf::st_write(gcp_qc, gcp_shp, delete_layer = TRUE, quiet = TRUE))
      ground_reference_files <- c(ground_reference_files, gcp_csv, gcp_shp)

      apply_bias <- as_bool(p$gcp_apply_bias_correction, TRUE)
      if (apply_bias) {
        dtm_corrected <- dtm_raw - mean_bias
        dtm_corrected_path <- file.path(ground_dir, 'dtm_bias_corrected.tif')
        terra::writeRaster(dtm_corrected, dtm_corrected_path, overwrite = TRUE, wopt = list(gdal = c('COMPRESS=LZW', 'TILED=YES')))
        normalization_surface <- dtm_corrected
        ground_reference_files <- c(ground_reference_files, dtm_corrected_path)
        write_log('INFO', sprintf('Mean bias %+.4f m diterapkan ke seluruh DTM. SD dilaporkan untuk QC; aplikasi tidak menilai otomatis apakah bias cukup konsisten.', mean_bias))
      } else {
        normalization_surface <- dtm_raw
        write_log('WARNING', 'Mode GCP bias berjalan sebagai validasi-only: mean bias dihitung tetapi tidak diterapkan ke DTM.')
      }
      ground_reference_summary$gcp_valid_count <- length(residual)
      ground_reference_summary$mean_bias_m <- mean_bias
      ground_reference_summary$sd_bias_m <- sd_bias
      ground_reference_summary$rmse_m <- rmse_bias
      ground_reference_summary$bias_correction_applied <- apply_bias
      ground_reference_summary$dtm_resolution_m <- ground_res
    }

    if (ground_mode == 'gcp_anchor') {
      ground_res <- as_num(p$ground_dtm_resolution_m, 0.5)
      ground_las <- lidR::filter_ground(las)
      if (lidR::is.empty(ground_las)) stop_app('Ground hasil CSF kosong; GCP anchor tidak dapat membentuk TIN gabungan.')
      ground_xyz <- data.frame(X = ground_las$X, Y = ground_las$Y, Z = ground_las$Z, Classification = 2L)
      gcp_coords <- sf::st_coordinates(validated$gcp)
      gcp_xyz <- data.frame(
        X = as.numeric(gcp_coords[, 1]),
        Y = as.numeric(gcp_coords[, 2]),
        Z = as.numeric(validated$gcp[[validated$gcp_elevation_field]]),
        Classification = 2L
      )
      ref_df <- rbind(ground_xyz, gcp_xyz)
      ref_las <- lidR::LAS(ref_df, crs = validated$las_crs)
      anchor_dtm <- lidR::rasterize_terrain(ref_las, res = ground_res, algorithm = lidR::tin())
      if (!inherits(anchor_dtm, 'SpatRaster')) anchor_dtm <- terra::rast(anchor_dtm)
      anchor_dtm_path <- file.path(ground_dir, 'dtm_csf_plus_gcp_anchor.tif')
      terra::writeRaster(anchor_dtm, anchor_dtm_path, overwrite = TRUE, wopt = list(gdal = c('COMPRESS=LZW', 'TILED=YES')))
      ground_reference_files <- c(ground_reference_files, anchor_dtm_path)
      normalization_surface <- anchor_dtm

      z_anchor <- as.numeric(terra::extract(anchor_dtm, terra::vect(validated$gcp), ID = FALSE)[, 1])
      z_ground <- as.numeric(validated$gcp[[validated$gcp_elevation_field]])
      valid_anchor <- is.finite(z_anchor) & is.finite(z_ground)
      residual_anchor <- z_anchor[valid_anchor] - z_ground[valid_anchor]
      gcp_anchor_qc <- validated$gcp[valid_anchor, , drop = FALSE]
      gcp_anchor_qc$z_anchor <- z_anchor[valid_anchor]
      gcp_anchor_qc$z_ground <- z_ground[valid_anchor]
      gcp_anchor_qc$resid <- residual_anchor
      anchor_csv <- file.path(ground_dir, 'gcp_anchor_validation.csv')
      utils::write.csv(sf::st_drop_geometry(gcp_anchor_qc), anchor_csv, row.names = FALSE, na = '')
      anchor_shp <- file.path(ground_dir, 'gcp_anchor_validation.shp')
      suppressWarnings(sf::st_write(gcp_anchor_qc, anchor_shp, delete_layer = TRUE, quiet = TRUE))
      ground_reference_files <- c(ground_reference_files, anchor_csv, anchor_shp)
      ground_reference_summary$gcp_valid_count <- sum(valid_anchor)
      ground_reference_summary$anchor_residual_mean_m <- if (length(residual_anchor)) mean(residual_anchor) else NA_real_
      ground_reference_summary$anchor_residual_sd_m <- if (length(residual_anchor) > 1) stats::sd(residual_anchor) else NA_real_
      ground_reference_summary$anchor_residual_rmse_m <- if (length(residual_anchor)) sqrt(mean(residual_anchor^2)) else NA_real_
      ground_reference_summary$dtm_resolution_m <- ground_res
      write_log('INFO', sprintf('TIN ground anchor dibentuk dari %s ground CSF + %s GCP pada raster %.3f m.',
                                format(nrow(ground_xyz), big.mark = ','), nrow(gcp_xyz), ground_res))
    }
  }

  ground_summary_path <- write_ground_reference_summary(ground_reference_summary, ground_dir)
  ground_reference_files <- unique(c(ground_reference_files, ground_summary_path))

  set_stage('normalize', 'Menormalisasi tinggi point cloud', 50)
  if (ground_mode == 'csf_tin') {
    nlas <- lidR::normalize_height(las, lidR::tin())
  } else {
    nlas <- lidR::normalize_height(las, normalization_surface)
  }
  nlas <- lidR::filter_poi(nlas, is.finite(Z) & Z >= -0.1)
  if (lidR::is.empty(nlas)) stop_app('Point cloud ternormalisasi kosong.')
  normalized_points <- lidR::npoints(nlas)
  if (ground_mode != 'external_dtm') {
    gnd <- lidR::filter_ground(nlas)
    if (!lidR::is.empty(gnd)) {
      write_log('INFO', sprintf('Ground Z setelah normalisasi: min %.4f; mean %.4f; max %.4f', min(gnd$Z), mean(gnd$Z), max(gnd$Z)))
    }
  }
  normalized_laz <- ''
  if (as_bool(p$save_normalized_laz, TRUE)) {
    normalized_laz <- file.path(run_dir, 'normalized_pointcloud.laz')
    lidR::writeLAS(nlas, normalized_laz)
  }

  set_stage('chm', 'Membuat normalized Canopy Height Model', 62)
  chm <- lidR::rasterize_canopy(
    nlas,
    res = chm_res,
    algorithm = lidR::pitfree(thresholds = c(0, 2, 5, 10), max_edge = c(0, 1.5))
  )
  if (!inherits(chm, 'SpatRaster')) chm <- terra::rast(chm)
  chm_path <- file.path(run_dir, 'nCHM_sawit.tif')
  terra::writeRaster(chm, chm_path, overwrite = TRUE, wopt = list(gdal = c('COMPRESS=LZW', 'TILED=YES')))
  na_cells <- as.numeric(terra::global(is.na(chm), 'sum', na.rm = TRUE)[1, 1])
  na_pct <- na_cells / terra::ncell(chm) * 100
  write_log('INFO', sprintf('CHM: resolusi %.3f m; piksel NA %.2f%%', chm_res, na_pct))
  if (na_pct > 10) write_log('WARNING', 'Piksel NA CHM lebih dari 10%. Pertimbangkan resolusi CHM lebih kasar.')
  chm_png <- file.path(run_dir, 'qc_nchm.png')
  try_qc_png(chm_png, 'pratinjau nCHM', terra::plot(chm, main = 'Normalized Canopy Height Model (m)'))
  histogram_png <- file.path(run_dir, 'qc_histogram_height.png')
  try_qc_png(histogram_png, 'histogram tinggi', terra::hist(chm, breaks = 60, main = 'Distribusi Nilai nCHM', xlab = 'Tinggi (m)'))

  set_stage('zonal', 'Menghitung statistik tinggi pada buffer titik pokok', 75)
  points <- validated$points
  points <- sf::st_transform(points, sf::st_crs(terra::crs(chm, proj = TRUE)))
  id_field <- as.character(p$tree_id_field %||% '')
  if (nzchar(id_field)) {
    if (!id_field %in% names(points)) stop_app(paste('Field ID tidak ditemukan:', id_field))
    if (anyDuplicated(points[[id_field]])) write_log('WARNING', paste('Field ID tidak unik:', id_field))
  } else {
    id_field <- 'tree_id'
    while (id_field %in% names(points)) id_field <- paste0(id_field, '_x')
    points[[id_field]] <- seq_len(nrow(points))
  }

  # ID kanonik untuk output Shapefile monitoring. Selalu <=10 karakter.
  points$tree_id <- as.character(points[[id_field]])
  if (anyDuplicated(points$tree_id)) {
    if (validated$monitoring_mode == 'monitoring') stop_app('tree_id kanonik tidak unik; periksa Field ID pohon.')
    write_log('WARNING', 'tree_id kanonik tidak unik pada baseline; monitoring berikutnya akan membutuhkan ID yang benar-benar unik.')
  }

  buffer_m <- as_num(p$buffer_m, 2)
  buffers <- sf::st_buffer(points, dist = buffer_m)
  threshold <- as_num(p$canopy_threshold_m, 0.5)
  chm_masked <- terra::ifel(chm >= threshold, chm, NA)

  stat_fun <- function(values, coverage_fraction) {
    values <- as.numeric(values)
    coverage_fraction <- as.numeric(coverage_fraction)
    ok <- is.finite(values) & is.finite(coverage_fraction) & coverage_fraction > 0
    if (!any(ok)) {
      return(data.frame(
        tinggi_rerata = NA_real_, tinggi_maks = NA_real_, tinggi_min = NA_real_,
        tinggi_sd = NA_real_, n_piksel_kanopi = 0L, bobot_piksel_kanopi = 0
      ))
    }
    v <- values[ok]
    w <- coverage_fraction[ok]
    data.frame(
      tinggi_rerata = sum(v * w) / sum(w),
      tinggi_maks = max(v),
      tinggi_min = min(v),
      tinggi_sd = if (length(v) > 1) stats::sd(v) else 0,
      n_piksel_kanopi = length(v),
      bobot_piksel_kanopi = sum(w)
    )
  }

  height_stats <- exactextractr::exact_extract(
    chm_masked, buffers, fun = stat_fun,
    summarize_df = FALSE, progress = FALSE
  )
  observed_binary <- terra::ifel(is.na(chm), NA, terra::ifel(chm >= threshold, 1, 0))
  canopy_fraction <- exactextractr::exact_extract(observed_binary, buffers, fun = 'mean', progress = FALSE)
  available_pixels <- exactextractr::exact_extract(chm, buffers, fun = 'count', progress = FALSE)

  points$tinggi_rerata <- height_stats$tinggi_rerata
  points$tinggi_maks <- height_stats$tinggi_maks
  points$tinggi_min <- height_stats$tinggi_min
  points$tinggi_sd <- height_stats$tinggi_sd
  points$n_piksel_kanopi <- as.integer(height_stats$n_piksel_kanopi)
  points$bobot_piksel_kanopi <- height_stats$bobot_piksel_kanopi
  points$pct_tutupan_kanopi <- round(as.numeric(canopy_fraction) * 100, 2)
  points$n_piksel_tersedia <- as.numeric(available_pixels)

  # Kelas tinggi hanya digunakan pada periode pertama/baseline.
  break1 <- as_num(p$height_break_1_m, 1.5)
  break2 <- as_num(p$height_break_2_m, 2.5)
  points$kelas_tinggi <- ifelse(
    is.na(points$tinggi_rerata), 'Tidak tersedia',
    ifelse(points$tinggi_rerata < break1, 'Pendek (terhambat)',
      ifelse(points$tinggi_rerata < break2, 'Sedang (normal)', 'Tinggi (subur)'))
  )
  points$status_qc <- ifelse(
    is.na(points$tinggi_rerata) | points$n_piksel_tersedia <= 0, 'OUTSIDE_OR_NODATA',
    ifelse(points$n_piksel_kanopi < 10, 'LOW_SUPPORT',
      ifelse(points$pct_tutupan_kanopi < 20, 'LOW_CANOPY_COVER', 'OK'))
  )

  # ---- Periodisasi atribut untuk monitoring Shapefile ----
  period_code <- validated$period_code
  rerata_field <- period_field('rerata', period_code)
  maks_field <- period_field('maks', period_code)
  min_field <- period_field('min', period_code)
  sd_field <- period_field('sd', period_code)
  npix_field <- period_field('npix', period_code)
  cover_field <- period_field('cover', period_code)
  kelas_field <- period_field('kelas', period_code)
  qc_field <- period_field('qc', period_code)

  current_metrics <- data.frame(
    rerata = points$tinggi_rerata,
    maks = points$tinggi_maks,
    min = points$tinggi_min,
    sd = points$tinggi_sd,
    npix = points$n_piksel_kanopi,
    cover = points$pct_tutupan_kanopi,
    kelas = points$kelas_tinggi,
    qc = points$status_qc,
    stringsAsFactors = FALSE
  )

  # Buang nama metrik internal yang terlalu panjang untuk DBF, lalu buat nama aman <=10 karakter.
  internal_metric_fields <- c('tinggi_rerata','tinggi_maks','tinggi_min','tinggi_sd',
                              'n_piksel_kanopi','bobot_piksel_kanopi','pct_tutupan_kanopi',
                              'n_piksel_tersedia','kelas_tinggi','status_qc')
  keep_base <- setdiff(names(points), internal_metric_fields)
  points_out <- points[, keep_base, drop = FALSE]

  # Bawa seluruh atribut historis dari hasil monitoring sebelumnya berdasarkan ID pokok.
  growth_field <- ''
  previous_max_field <- ''
  matched_growth <- 0L
  if (validated$monitoring_mode == 'monitoring') {
    prev <- validated$previous_points
    prev_id <- validated$previous_id_field
    idx <- match(as.character(points_out$tree_id), as.character(prev[[prev_id]]))
    historical_fields <- names(prev)[vapply(names(prev), is_monitoring_field, logical(1))]
    for (nm in historical_fields) {
      if (!nm %in% names(points_out)) points_out[[nm]] <- prev[[nm]][idx]
    }
    previous_max_field <- period_field('maks', validated$previous_period_code)
  }

  points_out[[rerata_field]] <- current_metrics$rerata
  points_out[[maks_field]] <- current_metrics$maks
  points_out[[min_field]] <- current_metrics$min
  points_out[[sd_field]] <- current_metrics$sd
  points_out[[npix_field]] <- current_metrics$npix
  points_out[[cover_field]] <- current_metrics$cover
  points_out[[qc_field]] <- current_metrics$qc
  points_out$periode <- period_code

  if (validated$monitoring_mode == 'monitoring') {
    growth_field <- period_field('tumb', period_code)
    previous_max <- suppressWarnings(as.numeric(points_out[[previous_max_field]]))
    current_max <- suppressWarnings(as.numeric(points_out[[maks_field]]))
    points_out[[growth_field]] <- current_max - previous_max
    matched_growth <- sum(is.finite(points_out[[growth_field]]))
    write_log('INFO', sprintf('Pertumbuhan %s dihitung untuk %d pokok: %s - %s.',
                              growth_field, matched_growth, maks_field, previous_max_field))
    missing_growth <- nrow(points_out) - matched_growth
    if (missing_growth > 0) write_log('WARNING', paste(missing_growth, 'pokok tidak memiliki pasangan tinggi maksimum valid; pertumbuhan = NA.'))

    # Mulai D2, kelas_Dx bukan lagi kelas tinggi. Nilainya menjadi status pertumbuhan
    # kualitatif berdasarkan delta tinggi maksimum terhadap periode sebelumnya.
    growth_normal_min <- as_num(p$growth_normal_min_m, 0.10)
    delta <- suppressWarnings(as.numeric(points_out[[growth_field]]))
    growth_status <- ifelse(
      !is.finite(delta), 'PERLU CEK',
      ifelse(delta < 0, 'PERLU CEK',
        ifelse(delta >= growth_normal_min, 'NORMAL', 'ANOMALI'))
    )
    points_out[[kelas_field]] <- growth_status
    write_log('INFO', sprintf('Status %s: NORMAL jika delta >= %.3f m; ANOMALI jika 0 <= delta < %.3f m; PERLU CEK jika delta negatif/tidak tersedia.',
                              kelas_field, growth_normal_min, growth_normal_min))
  } else {
    # D1 / baseline: kelas_D1 tetap berisi kelas tinggi absolut.
    points_out[[kelas_field]] <- current_metrics$kelas
  }

  buffers_out <- buffers
  buffers_out$tree_id <- points_out$tree_id
  buffers_out[[rerata_field]] <- points_out[[rerata_field]]
  buffers_out[[maks_field]] <- points_out[[maks_field]]
  buffers_out[[qc_field]] <- points_out[[qc_field]]
  if (nzchar(growth_field)) buffers_out[[growth_field]] <- points_out[[growth_field]]
  buffers_out$periode <- period_code

  set_stage('export', 'Mengekspor CSV, Shapefile, raster, dan laporan', 88)
  csv_path <- file.path(run_dir, paste0('tinggi_pokok_sawit_', period_code, '.csv'))
  utils::write.csv(sf::st_drop_geometry(points_out), csv_path, row.names = FALSE, na = '')

  shp_dir <- file.path(run_dir, 'shapefile')
  dir.create(shp_dir, showWarnings = FALSE)
  point_shp <- file.path(shp_dir, paste0('hasil_tinggi_pokok_', period_code, '.shp'))
  buffer_shp <- file.path(shp_dir, paste0('buffer_pokok_', period_code, '.shp'))
  suppressWarnings(sf::st_write(points_out, point_shp, delete_layer = TRUE, quiet = TRUE))
  suppressWarnings(sf::st_write(buffers_out, buffer_shp, delete_layer = TRUE, quiet = TRUE))
  shapefile_paths <- c(point_shp, buffer_shp)
  write_log('INFO', paste('Output utama Shapefile:', point_shp))

  preview_cols <- unique(c('tree_id', rerata_field, maks_field, min_field, cover_field, kelas_field, qc_field, if (nzchar(growth_field)) growth_field else character(0)))
  preview <- head(sf::st_drop_geometry(points_out)[, preview_cols, drop = FALSE], 20)
  valid_height_count <- sum(is.finite(points_out[[rerata_field]]))
  class_table <- as.list(table(points_out[[kelas_field]], useNA = 'ifany'))
  qc_table <- as.list(table(points_out[[qc_field]], useNA = 'ifany'))

  outputs <- c(
    config_path, log_path, chm_path, csv_path, density_png, chm_png, histogram_png, ground_reference_files,
    if (nzchar(normalized_laz)) normalized_laz else character(0), shapefile_paths
  )
  outputs <- unique(outputs[file.exists(outputs)])
  manifest <- data.frame(
    file = basename(outputs),
    path = normalizePath(outputs, winslash = '/', mustWork = FALSE),
    exists = file.exists(outputs),
    size_mb = round(ifelse(file.exists(outputs), file.info(outputs)$size / 1024^2, NA_real_), 3)
  )
  manifest_path <- file.path(run_dir, 'output_manifest.csv')
  utils::write.csv(manifest, manifest_path, row.names = FALSE)

  summary <- list(
    status = 'success',
    run_name = basename(run_dir),
    run_dir = run_dir,
    input_points = input_points,
    normalized_points = normalized_points,
    avg_density_points_m2 = avg_density,
    avg_spacing_m = avg_spacing,
    chm_resolution_m = chm_res,
    chm_na_pct = na_pct,
    ground_reference_mode = ground_mode,
    ground_reference_label = ground_label,
    ground_reference = ground_reference_summary,
    ground_pct = ground_pct,
    tree_count = nrow(points_out),
    valid_height_count = valid_height_count,
    monitoring_mode = validated$monitoring_mode,
    period_code = period_code,
    previous_period_code = validated$previous_period_code,
    matched_previous_count = validated$matched_previous_count,
    growth_field = growth_field,
    growth_valid_count = matched_growth,
    growth_normal_min_m = as_num(p$growth_normal_min_m, 0.10),
    class_semantics = if (validated$monitoring_mode == 'monitoring') 'growth_status' else 'height_class',
    class_counts = class_table,
    qc_counts = qc_table,
    output_files = c(outputs, manifest_path),
    preview = preview
  )
  result_json <- file.path(run_dir, 'result_summary.json')
  jsonlite::write_json(summary, result_json, pretty = TRUE, auto_unbox = TRUE, na = 'null')
  report_path <- file.path(run_dir, 'report.html')
  make_html_report(summary, preview, report_path)
  summary$output_files <- c(summary$output_files, result_json, report_path)
  jsonlite::write_json(summary, result_json, pretty = TRUE, auto_unbox = TRUE, na = 'null')

  set_stage('complete', 'Analisis selesai', 100)
  emit('result', status = 'success', runDir = run_dir, summaryPath = result_json,
       reportPath = report_path, summary = summary)
  write_log('INFO', 'Analisis selesai tanpa fatal error.')
}

status <- 0L
withCallingHandlers(
  tryCatch({
    if (mode == 'validate') {
      validate_common(FALSE)
      set_stage('complete', 'Validasi berhasil', 100)
      emit('validation-result', status = 'success', message = 'Semua input utama valid untuk diproses.')
    } else {
      run_pipeline()
    }
  }, error = function(e) {
    status <<- 10L
    message <- conditionMessage(e)
    try(cat(sprintf('[%s] [FATAL] [%s] %s\n', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), current_stage, message),
            file = log_path, append = TRUE), silent = TRUE)
    emit('fatal', stage = current_stage, message = message, runDir = run_dir, logPath = log_path)
  }),
  warning = function(w) {
    message <- conditionMessage(w)
    try(write_log('WARNING', message), silent = TRUE)
    invokeRestart('muffleWarning')
  }
)
quit(status = status)
