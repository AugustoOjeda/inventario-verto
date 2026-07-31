PAQUETES_DATOS <- c("DBI", "RPostgres", "pool")
PAQUETES_WORD <- c("xml2")
PAQUETES_BACKUP <- c("writexl")

CATEGORIAS_INICIALES <- c(
  "Equipos",
  "Material de vidrio y laboratorio",
  "Material plástico y descartable",
  "Instrumental y útiles",
  "Elementos de seguridad",
  "Reactivos"
)

# Entorno privado para conservar un único pool durante la ejecución.
.inventario_verto_estado <- new.env(parent = emptyenv())
.inventario_verto_estado$pool <- NULL
.inventario_verto_estado$onstop_registrado <- FALSE

# ------------------------------------------------------------
# Utilidades generales
# ------------------------------------------------------------

verificar_paquetes <- function(paquetes, contexto = "el programa") {
  faltantes <- paquetes[
    !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(faltantes) > 0) {
    stop(
      paste0(
        "Faltan paquetes necesarios para ", contexto, ": ",
        paste(faltantes, collapse = ", "),
        ". Instalalos con: install.packages(c(",
        paste(sprintf('"%s"', faltantes), collapse = ", "),
        "))"
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

verificar_paquetes_datos <- function() {
  verificar_paquetes(PAQUETES_DATOS, "conectarse con Neon")
}

verificar_paquete_backup <- function() {
  verificar_paquetes(PAQUETES_BACKUP, "crear backups en Excel")
}

limpiar_texto <- function(x) {
  if (is.null(x) || length(x) == 0) return("")

  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("[\r\n\t]+", " ", x)
  x <- trimws(gsub("\\s+", " ", x, perl = TRUE))

  if (length(x) == 1) x[[1]] else x
}

numero_o_na <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(NA_real_)
  suppressWarnings(as.numeric(x))
}

booleano_seguro <- function(x) {
  isTRUE(as.logical(x))
}

# Se conserva esta función para que el main.R actual siga funcionando.
# En la versión web ya no se crea Stock_data: la base es Neon.
crear_rutas_programa <- function(directorio_programa) {
  directorio_programa <- normalizePath(
    directorio_programa,
    winslash = "/",
    mustWork = FALSE
  )

  list(
    programa = directorio_programa,
    stock_data = NULL,
    backups = NULL,
    base_datos = "Neon PostgreSQL"
  )
}

# ------------------------------------------------------------
# Configuración y pool de Neon PostgreSQL
# ------------------------------------------------------------

parsear_query_url <- function(query) {
  if (is.null(query) || !nzchar(query)) return(list())

  partes <- strsplit(query, "&", fixed = TRUE)[[1]]
  resultado <- list()

  for (parte in partes) {
    if (!nzchar(parte)) next

    posicion <- regexpr("=", parte, fixed = TRUE)

    if (posicion[[1]] < 0) {
      clave <- utils::URLdecode(parte)
      valor <- ""
    } else {
      clave <- utils::URLdecode(substr(parte, 1, posicion[[1]] - 1))
      valor <- utils::URLdecode(substr(parte, posicion[[1]] + 1, nchar(parte)))
    }

    if (nzchar(clave)) resultado[[clave]] <- valor
  }

  resultado
}

parsear_database_url <- function(database_url) {
  database_url <- limpiar_texto(database_url)

  if (!nzchar(database_url)) {
    stop("La variable DATABASE_URL está vacía.", call. = FALSE)
  }

  patron <- paste0(
    "^postgres(?:ql)?://",
    "([^:]+):([^@]*)@",
    "([^/:?#]+)",
    "(?::([0-9]+))?",
    "/([^?]+)",
    "(?:\\?(.*))?$"
  )

  coincidencia <- regexec(patron, database_url, perl = TRUE)
  partes <- regmatches(database_url, coincidencia)[[1]]

  if (length(partes) == 0) {
    stop(
      paste0(
        "DATABASE_URL no tiene un formato PostgreSQL válido. ",
        "Usá la cadena de conexión entregada por Neon."
      ),
      call. = FALSE
    )
  }

  puerto <- if (nzchar(partes[[5]])) {
    suppressWarnings(as.integer(partes[[5]]))
  } else {
    5432L
  }

  parametros <- parsear_query_url(
    if (length(partes) >= 7) partes[[7]] else ""
  )

  list(
    user = utils::URLdecode(partes[[2]]),
    password = utils::URLdecode(partes[[3]]),
    host = partes[[4]],
    port = puerto,
    dbname = utils::URLdecode(partes[[6]]),
    parametros = parametros
  )
}

obtener_configuracion_conexion <- function() {
  database_url <- Sys.getenv("DATABASE_URL", unset = "")

  if (nzchar(database_url)) {
    configuracion <- parsear_database_url(database_url)

    opciones_permitidas <- c(
      "sslmode",
      "channel_binding",
      "connect_timeout",
      "application_name",
      "target_session_attrs",
      "options",
      "keepalives",
      "keepalives_idle",
      "keepalives_interval",
      "keepalives_count"
    )

    extras <- configuracion$parametros[
      intersect(names(configuracion$parametros), opciones_permitidas)
    ]

    if (is.null(extras$sslmode) || !nzchar(extras$sslmode)) {
      extras$sslmode <- "require"
    }

    if (is.null(extras$application_name) || !nzchar(extras$application_name)) {
      extras$application_name <- "inventario_verto"
    }

    return(c(
      configuracion[c("dbname", "host", "port", "user", "password")],
      extras
    ))
  }

  variables <- c(
    host = "PGHOST",
    port = "PGPORT",
    dbname = "PGDATABASE",
    user = "PGUSER",
    password = "PGPASSWORD"
  )

  valores <- lapply(variables, Sys.getenv, unset = "")
  faltantes <- names(valores)[!vapply(valores, nzchar, logical(1))]

  if (length(faltantes) > 0) {
    stop(
      paste0(
        "No se encontró DATABASE_URL y faltan variables PostgreSQL: ",
        paste(unname(variables[faltantes]), collapse = ", "),
        ". Configurá DATABASE_URL con la cadena de conexión de Neon."
      ),
      call. = FALSE
    )
  }

  list(
    host = valores$host,
    port = suppressWarnings(as.integer(valores$port)),
    dbname = valores$dbname,
    user = valores$user,
    password = valores$password,
    sslmode = Sys.getenv("PGSSLMODE", unset = "require"),
    application_name = "inventario_verto"
  )
}

crear_pool_datos <- function(intentos = 4L, espera_segundos = 2) {
  verificar_paquetes_datos()

  if (!is.null(.inventario_verto_estado$pool)) {
    return(.inventario_verto_estado$pool)
  }

  configuracion <- obtener_configuracion_conexion()
  ultimo_error <- NULL

  for (intento in seq_len(as.integer(intentos))) {
    argumentos <- c(
      list(
        drv = RPostgres::Postgres(),
        bigint = "integer",
        timezone = "UTC",
        timezone_out = Sys.timezone(),
        minSize = 0,
        maxSize = 5,
        idleTimeout = 60,
        validationInterval = 30,
        validateQuery = "SELECT 1"
      ),
      configuracion
    )

    pool_creado <- NULL

    resultado <- tryCatch(
      {
        pool_creado <- do.call(pool::dbPool, argumentos)
        DBI::dbGetQuery(pool_creado, "SELECT 1 AS conexion")
        TRUE
      },
      error = function(e) {
        ultimo_error <<- e
        FALSE
      }
    )

    if (isTRUE(resultado)) {
      .inventario_verto_estado$pool <- pool_creado
      return(pool_creado)
    }

    if (!is.null(pool_creado)) {
      try(pool::poolClose(pool_creado), silent = TRUE)
    }

    if (intento < intentos) {
      Sys.sleep(espera_segundos * intento)
    }
  }

  stop(
    paste0(
      "No se pudo conectar con Neon después de ", intentos,
      " intentos. Detalle: ",
      if (is.null(ultimo_error)) "error desconocido" else conditionMessage(ultimo_error)
    ),
    call. = FALSE
  )
}

obtener_pool_datos <- function() {
  if (is.null(.inventario_verto_estado$pool)) {
    crear_pool_datos()
  } else {
    .inventario_verto_estado$pool
  }
}

# Alias compatible con el main.R anterior.
abrir_base_datos <- function(ruta_db = NULL) {
  obtener_pool_datos()
}

cerrar_pool_datos <- function() {
  if (!is.null(.inventario_verto_estado$pool)) {
    try(pool::poolClose(.inventario_verto_estado$pool), silent = TRUE)
    .inventario_verto_estado$pool <- NULL
  }

  invisible(TRUE)
}

probar_conexion_datos <- function() {
  pool_datos <- obtener_pool_datos()

  tryCatch(
    {
      resultado <- DBI::dbGetQuery(
        pool_datos,
        "SELECT NOW() AS servidor, current_database() AS base_datos"
      )

      list(
        conectado = TRUE,
        mensaje = "Conectado con Neon",
        detalle = resultado
      )
    },
    error = function(e) {
      list(
        conectado = FALSE,
        mensaje = conditionMessage(e),
        detalle = NULL
      )
    }
  )
}

# ------------------------------------------------------------
# Esquema PostgreSQL, revisión e historial
# ------------------------------------------------------------

registrar_cambio <- function(
  con,
  accion,
  entidad = "sistema",
  entidad_id = NA_integer_,
  detalle = ""
) {
  revision <- DBI::dbGetQuery(
    con,
    paste0(
      "UPDATE app_estado ",
      "SET revision = revision + 1, actualizado_en = NOW() ",
      "WHERE id = 1 RETURNING revision;"
    )
  )$revision[[1]]

  DBI::dbExecute(
    con,
    paste0(
      "INSERT INTO historial(",
      "revision, accion, entidad, entidad_id, detalle, creado_en",
      ") VALUES ($1, $2, $3, $4, $5, NOW());"
    ),
    params = list(
      as.integer(revision),
      limpiar_texto(accion),
      limpiar_texto(entidad),
      if (is.na(entidad_id)) NA_integer_ else as.integer(entidad_id),
      limpiar_texto(detalle)
    )
  )

  as.integer(revision)
}

inicializar_base_datos <- function(ruta_db = NULL) {
  pool_datos <- obtener_pool_datos()

  pool::poolWithTransaction(pool_datos, function(con) {
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS categorias (
        id INTEGER GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
        nombre TEXT NOT NULL,
        orden INTEGER NOT NULL DEFAULT 0,
        creado_en TIMESTAMPTZ NOT NULL DEFAULT NOW()
      );
    ")

    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS materiales (
        id INTEGER GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
        nombre TEXT NOT NULL,
        categoria_id INTEGER NOT NULL REFERENCES categorias(id),
        comentarios TEXT NOT NULL DEFAULT '',
        observaciones TEXT NOT NULL DEFAULT '',
        es_compra BOOLEAN NOT NULL DEFAULT FALSE,
        cantidad_compra TEXT NOT NULL DEFAULT '',
        precio_compra TEXT NOT NULL DEFAULT '',
        origen TEXT NOT NULL DEFAULT 'manual',
        creado_en TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        actualizado_en TIMESTAMPTZ NOT NULL DEFAULT NOW()
      );
    ")

    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS stock (
        id INTEGER GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
        material_id INTEGER NOT NULL REFERENCES materiales(id) ON DELETE CASCADE,
        cantidad DOUBLE PRECISION,
        unidad TEXT NOT NULL DEFAULT 'unidad',
        contenido_por_unidad DOUBLE PRECISION,
        detalle TEXT NOT NULL DEFAULT '',
        orden INTEGER NOT NULL DEFAULT 0,
        creado_en TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        actualizado_en TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        CONSTRAINT stock_cantidad_no_negativa
          CHECK (cantidad IS NULL OR cantidad >= 0),
        CONSTRAINT stock_contenido_positivo
          CHECK (contenido_por_unidad IS NULL OR contenido_por_unidad > 0)
      );
    ")

    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS app_estado (
        id SMALLINT PRIMARY KEY CHECK (id = 1),
        revision INTEGER NOT NULL DEFAULT 0,
        actualizado_en TIMESTAMPTZ NOT NULL DEFAULT NOW()
      );
    ")

    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS historial (
        id INTEGER GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
        revision INTEGER NOT NULL,
        accion TEXT NOT NULL,
        entidad TEXT NOT NULL,
        entidad_id INTEGER,
        detalle TEXT NOT NULL DEFAULT '',
        creado_en TIMESTAMPTZ NOT NULL DEFAULT NOW()
      );
    ")

    # Migraciones seguras para versiones PostgreSQL anteriores.
    DBI::dbExecute(
      con,
      "ALTER TABLE materiales ADD COLUMN IF NOT EXISTS es_compra BOOLEAN NOT NULL DEFAULT FALSE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE materiales ADD COLUMN IF NOT EXISTS cantidad_compra TEXT NOT NULL DEFAULT '';"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE materiales ADD COLUMN IF NOT EXISTS precio_compra TEXT NOT NULL DEFAULT '';"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE materiales ADD COLUMN IF NOT EXISTS origen TEXT NOT NULL DEFAULT 'manual';"
    )

    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS ux_categorias_nombre_ci ON categorias ((LOWER(nombre)));"
    )
    DBI::dbExecute(
      con,
      paste0(
        "CREATE UNIQUE INDEX IF NOT EXISTS ux_materiales_nombre_categoria_ci ",
        "ON materiales (categoria_id, (LOWER(nombre)));"
      )
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS idx_material_categoria ON materiales(categoria_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS idx_stock_material ON stock(material_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS idx_historial_fecha ON historial(creado_en DESC);"
    )

    DBI::dbExecute(
      con,
      paste0(
        "INSERT INTO app_estado(id, revision, actualizado_en) ",
        "VALUES (1, 0, NOW()) ON CONFLICT (id) DO NOTHING;"
      )
    )

    for (i in seq_along(CATEGORIAS_INICIALES)) {
      DBI::dbExecute(
        con,
        paste0(
          "INSERT INTO categorias(nombre, orden, creado_en) ",
          "VALUES ($1, $2, NOW()) ON CONFLICT DO NOTHING;"
        ),
        params = list(CATEGORIAS_INICIALES[[i]], as.integer(i))
      )
    }
  })

  if (
    requireNamespace("shiny", quietly = TRUE) &&
      !isTRUE(.inventario_verto_estado$onstop_registrado)
  ) {
    try(
      shiny::onStop(function() cerrar_pool_datos()),
      silent = TRUE
    )
    .inventario_verto_estado$onstop_registrado <- TRUE
  }

  invisible(TRUE)
}

obtener_revision <- function(ruta_db = NULL) {
  pool_datos <- obtener_pool_datos()
  resultado <- DBI::dbGetQuery(
    pool_datos,
    "SELECT revision FROM app_estado WHERE id = 1;"
  )

  if (nrow(resultado) == 0) 0L else as.integer(resultado$revision[[1]])
}

obtener_historial <- function(ruta_db = NULL, limite = 500L) {
  pool_datos <- obtener_pool_datos()

  DBI::dbGetQuery(
    pool_datos,
    paste0(
      "SELECT id, revision, accion, entidad, entidad_id, detalle, creado_en ",
      "FROM historial ORDER BY creado_en DESC, id DESC LIMIT $1;"
    ),
    params = list(as.integer(limite))
  )
}

# ------------------------------------------------------------
# Categorías
# ------------------------------------------------------------

obtener_categorias <- function(ruta_db = NULL) {
  pool_datos <- obtener_pool_datos()

  DBI::dbGetQuery(
    pool_datos,
    "SELECT id, nombre, orden FROM categorias ORDER BY orden, LOWER(nombre), id;"
  )
}

agregar_categoria <- function(ruta_db = NULL, nombre) {
  nombre <- limpiar_texto(nombre)
  if (!nzchar(nombre)) stop("Escribí un nombre para la categoría.", call. = FALSE)

  pool_datos <- obtener_pool_datos()

  pool::poolWithTransaction(pool_datos, function(con) {
    existente <- DBI::dbGetQuery(
      con,
      "SELECT id FROM categorias WHERE LOWER(nombre) = LOWER($1) LIMIT 1;",
      params = list(nombre)
    )

    if (nrow(existente) > 0) return(as.integer(existente$id[[1]]))

    orden <- DBI::dbGetQuery(
      con,
      "SELECT COALESCE(MAX(orden), 0) + 1 AS siguiente FROM categorias;"
    )$siguiente[[1]]

    nueva <- DBI::dbGetQuery(
      con,
      paste0(
        "INSERT INTO categorias(nombre, orden, creado_en) ",
        "VALUES ($1, $2, NOW()) ON CONFLICT DO NOTHING RETURNING id;"
      ),
      params = list(nombre, as.integer(orden))
    )

    if (nrow(nueva) == 0) {
      nueva <- DBI::dbGetQuery(
        con,
        "SELECT id FROM categorias WHERE LOWER(nombre) = LOWER($1) LIMIT 1;",
        params = list(nombre)
      )
      return(as.integer(nueva$id[[1]]))
    }

    categoria_id <- as.integer(nueva$id[[1]])
    registrar_cambio(
      con,
      accion = "crear_categoria",
      entidad = "categoria",
      entidad_id = categoria_id,
      detalle = nombre
    )

    categoria_id
  })
}

eliminar_categoria <- function(ruta_db = NULL, categoria_id) {
  pool_datos <- obtener_pool_datos()
  categoria_id <- as.integer(categoria_id)

  pool::poolWithTransaction(pool_datos, function(con) {
    usados <- DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS cantidad FROM materiales WHERE categoria_id = $1;",
      params = list(categoria_id)
    )$cantidad[[1]]

    if (usados > 0) {
      stop("La categoría contiene materiales y no puede eliminarse.", call. = FALSE)
    }

    eliminada <- DBI::dbGetQuery(
      con,
      "DELETE FROM categorias WHERE id = $1 RETURNING nombre;",
      params = list(categoria_id)
    )

    if (nrow(eliminada) == 0) {
      stop("No se encontró la categoría.", call. = FALSE)
    }

    registrar_cambio(
      con,
      accion = "eliminar_categoria",
      entidad = "categoria",
      entidad_id = categoria_id,
      detalle = eliminada$nombre[[1]]
    )

    invisible(TRUE)
  })
}

# ------------------------------------------------------------
# Materiales
# ------------------------------------------------------------

agregar_material <- function(
  ruta_db = NULL,
  nombre,
  categoria_id,
  cantidad = 0,
  unidad = "unidad",
  contenido_por_unidad = NA_real_,
  comentarios = "",
  observaciones = "",
  detalle_stock = "",
  es_compra = FALSE,
  cantidad_compra = "",
  precio_compra = "",
  origen = "manual"
) {
  nombre <- limpiar_texto(nombre)
  unidad <- limpiar_texto(unidad)
  origen <- limpiar_texto(origen)
  cantidad <- numero_o_na(cantidad)
  contenido_por_unidad <- numero_o_na(contenido_por_unidad)

  if (!nzchar(nombre)) stop("Escribí el nombre del material.", call. = FALSE)
  if (!nzchar(unidad)) unidad <- "unidad"
  if (!nzchar(origen)) origen <- "manual"

  if (!is.na(cantidad) && cantidad < 0) {
    stop("La cantidad no puede ser negativa.", call. = FALSE)
  }

  if (!is.na(contenido_por_unidad) && contenido_por_unidad <= 0) {
    stop("El contenido por unidad debe ser mayor que cero.", call. = FALSE)
  }

  pool_datos <- obtener_pool_datos()

  pool::poolWithTransaction(pool_datos, function(con) {
    material <- DBI::dbGetQuery(
      con,
      paste0(
        "INSERT INTO materiales(",
        "nombre, categoria_id, comentarios, observaciones, ",
        "es_compra, cantidad_compra, precio_compra, origen, ",
        "creado_en, actualizado_en",
        ") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, NOW(), NOW()) ",
        "ON CONFLICT DO NOTHING RETURNING id;"
      ),
      params = list(
        nombre,
        as.integer(categoria_id),
        limpiar_texto(comentarios),
        limpiar_texto(observaciones),
        booleano_seguro(es_compra),
        limpiar_texto(cantidad_compra),
        limpiar_texto(precio_compra),
        origen
      )
    )

    if (nrow(material) == 0) {
      stop("Ese material ya existe en la categoría seleccionada.", call. = FALSE)
    }

    material_id <- as.integer(material$id[[1]])

    DBI::dbExecute(
      con,
      paste0(
        "INSERT INTO stock(",
        "material_id, cantidad, unidad, contenido_por_unidad, detalle, orden, ",
        "creado_en, actualizado_en",
        ") VALUES ($1, $2, $3, $4, $5, 1, NOW(), NOW());"
      ),
      params = list(
        material_id,
        cantidad,
        unidad,
        contenido_por_unidad,
        limpiar_texto(detalle_stock)
      )
    )

    registrar_cambio(
      con,
      accion = "crear_material",
      entidad = "material",
      entidad_id = material_id,
      detalle = nombre
    )

    material_id
  })
}

actualizar_material <- function(
  ruta_db = NULL,
  material_id,
  nombre,
  categoria_id,
  comentarios = "",
  observaciones = "",
  es_compra = NULL,
  cantidad_compra = NULL,
  precio_compra = NULL
) {
  material_id <- as.integer(material_id)
  nombre <- limpiar_texto(nombre)

  if (!nzchar(nombre)) stop("El nombre no puede estar vacío.", call. = FALSE)

  pool_datos <- obtener_pool_datos()

  pool::poolWithTransaction(pool_datos, function(con) {
    actual <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT nombre, es_compra, cantidad_compra, precio_compra ",
        "FROM materiales WHERE id = $1 LIMIT 1;"
      ),
      params = list(material_id)
    )

    if (nrow(actual) == 0) stop("No se encontró el material.", call. = FALSE)

    duplicado <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT id FROM materiales ",
        "WHERE LOWER(nombre) = LOWER($1) ",
        "AND categoria_id = $2 AND id <> $3 LIMIT 1;"
      ),
      params = list(nombre, as.integer(categoria_id), material_id)
    )

    if (nrow(duplicado) > 0) {
      stop("Ya existe otro material con ese nombre en esa categoría.", call. = FALSE)
    }

    es_compra_final <- if (is.null(es_compra)) {
      isTRUE(actual$es_compra[[1]])
    } else {
      booleano_seguro(es_compra)
    }

    cantidad_compra_final <- if (is.null(cantidad_compra)) {
      actual$cantidad_compra[[1]]
    } else {
      limpiar_texto(cantidad_compra)
    }

    precio_compra_final <- if (is.null(precio_compra)) {
      actual$precio_compra[[1]]
    } else {
      limpiar_texto(precio_compra)
    }

    DBI::dbExecute(
      con,
      paste0(
        "UPDATE materiales SET ",
        "nombre = $1, categoria_id = $2, comentarios = $3, observaciones = $4, ",
        "es_compra = $5, cantidad_compra = $6, precio_compra = $7, ",
        "actualizado_en = NOW() WHERE id = $8;"
      ),
      params = list(
        nombre,
        as.integer(categoria_id),
        limpiar_texto(comentarios),
        limpiar_texto(observaciones),
        es_compra_final,
        cantidad_compra_final,
        precio_compra_final,
        material_id
      )
    )

    registrar_cambio(
      con,
      accion = "editar_material",
      entidad = "material",
      entidad_id = material_id,
      detalle = paste0(actual$nombre[[1]], " -> ", nombre)
    )

    invisible(TRUE)
  })
}

actualizar_estado_compra <- function(
  ruta_db = NULL,
  material_id,
  es_compra,
  cantidad_compra = "",
  precio_compra = ""
) {
  material_id <- as.integer(material_id)
  pool_datos <- obtener_pool_datos()

  pool::poolWithTransaction(pool_datos, function(con) {
    actualizado <- DBI::dbGetQuery(
      con,
      paste0(
        "UPDATE materiales SET ",
        "es_compra = $1, cantidad_compra = $2, precio_compra = $3, ",
        "actualizado_en = NOW() WHERE id = $4 RETURNING nombre;"
      ),
      params = list(
        booleano_seguro(es_compra),
        limpiar_texto(cantidad_compra),
        limpiar_texto(precio_compra),
        material_id
      )
    )

    if (nrow(actualizado) == 0) stop("No se encontró el material.", call. = FALSE)

    registrar_cambio(
      con,
      accion = if (booleano_seguro(es_compra)) "marcar_compra" else "quitar_compra",
      entidad = "material",
      entidad_id = material_id,
      detalle = actualizado$nombre[[1]]
    )

    invisible(TRUE)
  })
}

marcar_para_comprar <- function(
  ruta_db = NULL,
  material_id,
  cantidad_compra = "",
  precio_compra = ""
) {
  actualizar_estado_compra(
    ruta_db = ruta_db,
    material_id = material_id,
    es_compra = TRUE,
    cantidad_compra = cantidad_compra,
    precio_compra = precio_compra
  )
}

quitar_marca_compra <- function(ruta_db = NULL, material_id) {
  actualizar_estado_compra(
    ruta_db = ruta_db,
    material_id = material_id,
    es_compra = FALSE,
    cantidad_compra = "",
    precio_compra = ""
  )
}

eliminar_material <- function(ruta_db = NULL, material_id) {
  material_id <- as.integer(material_id)
  pool_datos <- obtener_pool_datos()

  pool::poolWithTransaction(pool_datos, function(con) {
    eliminado <- DBI::dbGetQuery(
      con,
      "DELETE FROM materiales WHERE id = $1 RETURNING nombre;",
      params = list(material_id)
    )

    if (nrow(eliminado) == 0) stop("No se encontró el material.", call. = FALSE)

    registrar_cambio(
      con,
      accion = "eliminar_material",
      entidad = "material",
      entidad_id = material_id,
      detalle = eliminado$nombre[[1]]
    )

    invisible(TRUE)
  })
}

# ------------------------------------------------------------
# Stock
# ------------------------------------------------------------

agregar_stock <- function(
  ruta_db = NULL,
  material_id,
  cantidad = 0,
  unidad = "unidad",
  contenido_por_unidad = NA_real_,
  detalle = ""
) {
  material_id <- as.integer(material_id)
  cantidad <- numero_o_na(cantidad)
  contenido_por_unidad <- numero_o_na(contenido_por_unidad)
  unidad <- limpiar_texto(unidad)

  if (!nzchar(unidad)) unidad <- "unidad"
  if (!is.na(cantidad) && cantidad < 0) {
    stop("La cantidad no puede ser negativa.", call. = FALSE)
  }
  if (!is.na(contenido_por_unidad) && contenido_por_unidad <= 0) {
    stop("El contenido por unidad debe ser mayor que cero.", call. = FALSE)
  }

  pool_datos <- obtener_pool_datos()

  pool::poolWithTransaction(pool_datos, function(con) {
    orden <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT COALESCE(MAX(orden), 0) + 1 AS siguiente ",
        "FROM stock WHERE material_id = $1;"
      ),
      params = list(material_id)
    )$siguiente[[1]]

    nuevo <- DBI::dbGetQuery(
      con,
      paste0(
        "INSERT INTO stock(",
        "material_id, cantidad, unidad, contenido_por_unidad, detalle, orden, ",
        "creado_en, actualizado_en",
        ") VALUES ($1, $2, $3, $4, $5, $6, NOW(), NOW()) RETURNING id;"
      ),
      params = list(
        material_id,
        cantidad,
        unidad,
        contenido_por_unidad,
        limpiar_texto(detalle),
        as.integer(orden)
      )
    )

    stock_id <- as.integer(nuevo$id[[1]])

    registrar_cambio(
      con,
      accion = "agregar_stock",
      entidad = "stock",
      entidad_id = stock_id,
      detalle = paste0("material_id=", material_id)
    )

    stock_id
  })
}

ajustar_stock <- function(ruta_db = NULL, stock_id, cambio) {
  stock_id <- as.integer(stock_id)
  cambio <- suppressWarnings(as.numeric(cambio))

  if (is.na(cambio) || cambio == 0) return(invisible(FALSE))

  pool_datos <- obtener_pool_datos()

  pool::poolWithTransaction(pool_datos, function(con) {
    actualizado <- DBI::dbGetQuery(
      con,
      paste0(
        "UPDATE stock SET ",
        "cantidad = GREATEST(COALESCE(cantidad, 0) + $1, 0), ",
        "actualizado_en = NOW() ",
        "WHERE id = $2 RETURNING cantidad, material_id;"
      ),
      params = list(cambio, stock_id)
    )

    if (nrow(actualizado) == 0) stop("No se encontró ese stock.", call. = FALSE)

    registrar_cambio(
      con,
      accion = "ajustar_stock",
      entidad = "stock",
      entidad_id = stock_id,
      detalle = paste0("cambio=", cambio, "; cantidad=", actualizado$cantidad[[1]])
    )

    invisible(actualizado$cantidad[[1]])
  })
}

actualizar_stock <- function(
  ruta_db = NULL,
  stock_id,
  cantidad,
  unidad,
  contenido_por_unidad = NA_real_,
  detalle = ""
) {
  stock_id <- as.integer(stock_id)
  cantidad <- numero_o_na(cantidad)
  contenido_por_unidad <- numero_o_na(contenido_por_unidad)
  unidad <- limpiar_texto(unidad)

  if (!nzchar(unidad)) unidad <- "unidad"
  if (!is.na(cantidad) && cantidad < 0) {
    stop("La cantidad no puede ser negativa.", call. = FALSE)
  }
  if (!is.na(contenido_por_unidad) && contenido_por_unidad <= 0) {
    stop("El contenido por unidad debe ser mayor que cero.", call. = FALSE)
  }

  pool_datos <- obtener_pool_datos()

  pool::poolWithTransaction(pool_datos, function(con) {
    actualizado <- DBI::dbGetQuery(
      con,
      paste0(
        "UPDATE stock SET cantidad = $1, unidad = $2, contenido_por_unidad = $3, ",
        "detalle = $4, actualizado_en = NOW() ",
        "WHERE id = $5 RETURNING material_id;"
      ),
      params = list(
        cantidad,
        unidad,
        contenido_por_unidad,
        limpiar_texto(detalle),
        stock_id
      )
    )

    if (nrow(actualizado) == 0) stop("No se encontró ese stock.", call. = FALSE)

    registrar_cambio(
      con,
      accion = "editar_stock",
      entidad = "stock",
      entidad_id = stock_id,
      detalle = paste0("cantidad=", ifelse(is.na(cantidad), "S/C", cantidad))
    )

    invisible(TRUE)
  })
}

eliminar_stock <- function(ruta_db = NULL, stock_id) {
  stock_id <- as.integer(stock_id)
  pool_datos <- obtener_pool_datos()

  pool::poolWithTransaction(pool_datos, function(con) {
    eliminado <- DBI::dbGetQuery(
      con,
      "DELETE FROM stock WHERE id = $1 RETURNING material_id;",
      params = list(stock_id)
    )

    if (nrow(eliminado) == 0) stop("No se encontró ese stock.", call. = FALSE)

    registrar_cambio(
      con,
      accion = "eliminar_stock",
      entidad = "stock",
      entidad_id = stock_id,
      detalle = paste0("material_id=", eliminado$material_id[[1]])
    )

    invisible(TRUE)
  })
}

registrar_compra_realizada <- function(
  ruta_db = NULL,
  material_id,
  cantidad_recibida,
  unidad = "unidad",
  contenido_por_unidad = NA_real_,
  detalle = "Compra recibida",
  stock_id = NULL
) {
  material_id <- as.integer(material_id)
  cantidad_recibida <- numero_o_na(cantidad_recibida)
  contenido_por_unidad <- numero_o_na(contenido_por_unidad)
  unidad <- limpiar_texto(unidad)

  if (is.na(cantidad_recibida) || cantidad_recibida <= 0) {
    stop("La cantidad recibida debe ser mayor que cero.", call. = FALSE)
  }
  if (!nzchar(unidad)) unidad <- "unidad"
  if (!is.na(contenido_por_unidad) && contenido_por_unidad <= 0) {
    stop("El contenido por unidad debe ser mayor que cero.", call. = FALSE)
  }

  pool_datos <- obtener_pool_datos()

  pool::poolWithTransaction(pool_datos, function(con) {
    if (!is.null(stock_id) && !is.na(stock_id)) {
      stock_id <- as.integer(stock_id)
      actualizado <- DBI::dbGetQuery(
        con,
        paste0(
          "UPDATE stock SET cantidad = COALESCE(cantidad, 0) + $1, ",
          "actualizado_en = NOW() ",
          "WHERE id = $2 AND material_id = $3 RETURNING id;"
        ),
        params = list(cantidad_recibida, stock_id, material_id)
      )

      if (nrow(actualizado) == 0) {
        stop("No se encontró la presentación de stock seleccionada.", call. = FALSE)
      }
    } else {
      orden <- DBI::dbGetQuery(
        con,
        "SELECT COALESCE(MAX(orden), 0) + 1 AS siguiente FROM stock WHERE material_id = $1;",
        params = list(material_id)
      )$siguiente[[1]]

      nuevo <- DBI::dbGetQuery(
        con,
        paste0(
          "INSERT INTO stock(",
          "material_id, cantidad, unidad, contenido_por_unidad, detalle, orden, ",
          "creado_en, actualizado_en",
          ") VALUES ($1, $2, $3, $4, $5, $6, NOW(), NOW()) RETURNING id;"
        ),
        params = list(
          material_id,
          cantidad_recibida,
          unidad,
          contenido_por_unidad,
          limpiar_texto(detalle),
          as.integer(orden)
        )
      )
      stock_id <- as.integer(nuevo$id[[1]])
    }

    material <- DBI::dbGetQuery(
      con,
      paste0(
        "UPDATE materiales SET es_compra = FALSE, cantidad_compra = '', ",
        "precio_compra = '', actualizado_en = NOW() ",
        "WHERE id = $1 RETURNING nombre;"
      ),
      params = list(material_id)
    )

    if (nrow(material) == 0) stop("No se encontró el material.", call. = FALSE)

    registrar_cambio(
      con,
      accion = "compra_realizada",
      entidad = "material",
      entidad_id = material_id,
      detalle = paste0(material$nombre[[1]], "; recibido=", cantidad_recibida)
    )

    invisible(stock_id)
  })
}

# ------------------------------------------------------------
# Consultas
# ------------------------------------------------------------

obtener_inventario <- function(ruta_db = NULL) {
  pool_datos <- obtener_pool_datos()

  DBI::dbGetQuery(pool_datos, "
    WITH resumen_stock AS (
      SELECT
        material_id,
        COUNT(*) AS lineas_stock,
        BOOL_AND(cantidad IS NULL OR cantidad <= 0) AS sin_disponibilidad
      FROM stock
      GROUP BY material_id
    )
    SELECT
      c.id AS categoria_id,
      c.nombre AS categoria,
      c.orden AS categoria_orden,
      m.id AS material_id,
      m.nombre AS material,
      m.comentarios,
      m.observaciones,
      m.es_compra,
      m.cantidad_compra,
      m.precio_compra,
      m.origen,
      (
        m.es_compra
        OR COALESCE(rs.lineas_stock, 0) = 0
        OR COALESCE(rs.sin_disponibilidad, TRUE)
      ) AS requiere_compra,
      COALESCE(rs.lineas_stock, 0) AS lineas_stock,
      COALESCE(rs.sin_disponibilidad, TRUE) AS stock_sin_disponibilidad,
      s.id AS stock_id,
      s.cantidad,
      s.unidad,
      s.contenido_por_unidad,
      s.detalle AS detalle_stock,
      s.orden AS stock_orden
    FROM materiales m
    INNER JOIN categorias c ON c.id = m.categoria_id
    LEFT JOIN resumen_stock rs ON rs.material_id = m.id
    LEFT JOIN stock s ON s.material_id = m.id
    ORDER BY c.orden, LOWER(c.nombre), LOWER(m.nombre), s.orden, s.id;
  ")
}

obtener_material <- function(ruta_db = NULL, material_id) {
  pool_datos <- obtener_pool_datos()
  material_id <- as.integer(material_id)

  material <- DBI::dbGetQuery(
    pool_datos,
    paste0(
      "SELECT id, nombre, categoria_id, comentarios, observaciones, ",
      "es_compra, cantidad_compra, precio_compra, origen, creado_en, actualizado_en ",
      "FROM materiales WHERE id = $1 LIMIT 1;"
    ),
    params = list(material_id)
  )

  stock <- DBI::dbGetQuery(
    pool_datos,
    paste0(
      "SELECT id, cantidad, unidad, contenido_por_unidad, detalle, orden ",
      "FROM stock WHERE material_id = $1 ORDER BY orden, id;"
    ),
    params = list(material_id)
  )

  list(material = material, stock = stock)
}

formatear_cantidad <- function(cantidad, unidad, contenido_por_unidad = NA_real_) {
  if (is.na(cantidad)) {
    numero <- "S/c"
  } else if (abs(cantidad - round(cantidad)) < 1e-9) {
    numero <- as.character(as.integer(round(cantidad)))
  } else {
    numero <- format(
      cantidad,
      decimal.mark = ",",
      trim = TRUE,
      scientific = FALSE
    )
  }

  texto <- paste(numero, limpiar_texto(unidad))

  if (!is.na(contenido_por_unidad)) {
    contenido_texto <- if (
      abs(contenido_por_unidad - round(contenido_por_unidad)) < 1e-9
    ) {
      as.character(as.integer(round(contenido_por_unidad)))
    } else {
      format(
        contenido_por_unidad,
        decimal.mark = ",",
        trim = TRUE,
        scientific = FALSE
      )
    }

    texto <- paste0(texto, " de ", contenido_texto)
  }

  texto
}

# ------------------------------------------------------------
# Backup descargable
# ------------------------------------------------------------

crear_backup <- function(ruta_db = NULL, max_backups = 20) {
  pool_datos <- obtener_pool_datos()

  categorias <- DBI::dbGetQuery(
    pool_datos,
    "SELECT * FROM categorias ORDER BY orden, LOWER(nombre), id;"
  )
  materiales <- DBI::dbGetQuery(
    pool_datos,
    "SELECT * FROM materiales ORDER BY categoria_id, LOWER(nombre), id;"
  )
  stock <- DBI::dbGetQuery(
    pool_datos,
    "SELECT * FROM stock ORDER BY material_id, orden, id;"
  )
  inventario <- obtener_inventario()
  historial <- obtener_historial(limite = 10000L)

  datos_backup <- list(
    Inventario = inventario,
    Categorias = categorias,
    Materiales = materiales,
    Stock = stock,
    Historial = historial
  )

  nombre_base <- paste0(
    "Inventario_Verto_",
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    "_"
  )

  if (requireNamespace("writexl", quietly = TRUE)) {
    destino <- tempfile(pattern = nombre_base, fileext = ".xlsx")
    writexl::write_xlsx(datos_backup, path = destino)
    return(destino)
  }

  # Respaldo alternativo para que una importación Word no falle si
  # writexl todavía no está instalado. El formato RDS es portable entre
  # Windows, macOS y Linux y conserva todas las tablas.
  destino <- tempfile(pattern = nombre_base, fileext = ".rds")
  saveRDS(datos_backup, destino, compress = "xz")
  destino
}

# ============================================================
# IMPORTACIÓN DE INVENTARIOS DESDE WORD (.docx)
# ============================================================

verificar_paquete_word <- function() {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop(
      paste0(
        'Falta el paquete "xml2", necesario para importar archivos Word. ',
        'Instalalo con: install.packages("xml2")'
      ),
      call. = FALSE
    )
  }
}

normalizar_clave <- function(x) {
  x <- limpiar_texto(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  tolower(x)
}

normalizar_unidad_importada <- function(unidad) {
  unidad_original <- limpiar_texto(unidad)
  clave <- normalizar_clave(unidad_original)

  equivalencias <- c(
    "unidad" = "unidad",
    "unidades" = "unidad",
    "caja" = "caja",
    "cajas" = "caja",
    "par" = "par",
    "pares" = "par",
    "kit" = "kit",
    "kits" = "kit",
    "frasco" = "frasco",
    "frascos" = "frasco",
    "paquete" = "paquete",
    "paquetes" = "paquete",
    "bolsa" = "bolsa",
    "bolsas" = "bolsa",
    "rollo" = "rollo",
    "rollos" = "rollo",
    "botella" = "botella",
    "botellas" = "botella",
    "tubo" = "tubo",
    "tubos" = "tubo",
    "sobre" = "sobre",
    "sobres" = "sobre",
    "juego" = "juego",
    "juegos" = "juego",
    "set" = "set",
    "sets" = "set",
    "kg" = "kg",
    "g" = "g",
    "mg" = "mg",
    "l" = "L",
    "ml" = "mL",
    "ul" = "uL",
    "µl" = "µL"
  )

  if (clave %in% names(equivalencias)) {
    return(unname(equivalencias[[clave]]))
  }

  if (!nzchar(unidad_original)) "unidad" else unidad_original
}

interpretar_cantidad_word <- function(texto) {
  original <- limpiar_texto(texto)
  clave <- normalizar_clave(original)

  if (
    !nzchar(clave) ||
    clave %in% c("s/c", "s.c.", "sc", "sin cantidad", "no informado")
  ) {
    return(list(
      cantidad = NA_real_,
      unidad = "unidad",
      contenido_por_unidad = NA_real_,
      detalle = "Cantidad no informada"
    ))
  }

  aproximada <- grepl(
    "^(aprox\\.?|aproximadamente|aproximado)\\s*",
    clave,
    perl = TRUE
  )

  texto_trabajo <- sub(
    "^(aprox\\.?|aproximadamente|aproximado)\\s*",
    "",
    original,
    ignore.case = TRUE,
    perl = TRUE
  )
  texto_trabajo <- limpiar_texto(texto_trabajo)

  cantidad <- NA_real_
  resto <- ""

  # Fracciones como 1/2 kg.
  coincidencia_fraccion <- regexec(
    "^([0-9]+)\\s*/\\s*([0-9]+)(?:\\s+(.*))?$",
    texto_trabajo,
    perl = TRUE
  )
  partes_fraccion <- regmatches(texto_trabajo, coincidencia_fraccion)[[1]]

  if (length(partes_fraccion) > 0) {
    numerador <- suppressWarnings(as.numeric(partes_fraccion[[2]]))
    denominador <- suppressWarnings(as.numeric(partes_fraccion[[3]]))

    if (is.na(denominador) || denominador == 0) {
      stop(
        paste0("No se pudo interpretar la cantidad: ", original),
        call. = FALSE
      )
    }

    cantidad <- numerador / denominador
    resto <- if (length(partes_fraccion) >= 4) partes_fraccion[[4]] else ""
  } else {
    # Números enteros o decimales, con punto o coma.
    coincidencia_numero <- regexec(
      "^([0-9]+(?:[\\.,][0-9]+)?)(?:\\s+(.*))?$",
      texto_trabajo,
      perl = TRUE
    )
    partes_numero <- regmatches(texto_trabajo, coincidencia_numero)[[1]]

    if (length(partes_numero) == 0) {
      return(list(
        cantidad = NA_real_,
        unidad = "unidad",
        contenido_por_unidad = NA_real_,
        detalle = paste0("Cantidad original: ", original)
      ))
    }

    cantidad <- suppressWarnings(
      as.numeric(gsub(",", ".", partes_numero[[2]], fixed = TRUE))
    )
    resto <- if (length(partes_numero) >= 3) partes_numero[[3]] else ""
  }

  resto <- limpiar_texto(resto)
  contenido_por_unidad <- NA_real_
  unidad <- "unidad"
  detalle_partes <- character(0)

  if (nzchar(resto)) {
    # Ejemplo: “caja de 100”.
    coincidencia_contenido <- regexec(
      "^(.+?)\\s+de\\s+([0-9]+(?:[\\.,][0-9]+)?)$",
      resto,
      ignore.case = TRUE,
      perl = TRUE
    )
    partes_contenido <- regmatches(resto, coincidencia_contenido)[[1]]

    if (length(partes_contenido) > 0) {
      unidad <- normalizar_unidad_importada(partes_contenido[[2]])
      contenido_por_unidad <- suppressWarnings(
        as.numeric(gsub(",", ".", partes_contenido[[3]], fixed = TRUE))
      )
    } else {
      unidad <- normalizar_unidad_importada(resto)
    }
  }

  if (aproximada) {
    detalle_partes <- c(detalle_partes, "Cantidad aproximada")
  }

  list(
    cantidad = cantidad,
    unidad = unidad,
    contenido_por_unidad = contenido_por_unidad,
    detalle = paste(detalle_partes, collapse = ". ")
  )
}

texto_nodo_word <- function(nodo) {
  partes <- xml2::xml_find_all(
    nodo,
    ".//*[local-name()='t']"
  )

  if (length(partes) == 0) {
    return("")
  }

  textos <- xml2::xml_text(partes)
  textos <- vapply(textos, limpiar_texto, character(1))
  textos <- textos[nzchar(textos)]

  limpiar_texto(paste(textos, collapse = " "))
}

extraer_tablas_word_xml <- function(archivo_docx) {
  carpeta_temporal <- tempfile("inventario_verto_docx_")
  dir.create(carpeta_temporal, recursive = TRUE, showWarnings = FALSE)

  on.exit(
    unlink(carpeta_temporal, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  tryCatch(
    utils::unzip(
      archivo_docx,
      files = "word/document.xml",
      exdir = carpeta_temporal
    ),
    error = function(e) {
      stop(
        paste0(
          "No se pudo abrir internamente el archivo Word: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  ruta_xml <- file.path(
    carpeta_temporal,
    "word",
    "document.xml"
  )

  if (!file.exists(ruta_xml)) {
    stop(
      paste0(
        "El archivo seleccionado no contiene la estructura interna ",
        "de un documento .docx válido."
      ),
      call. = FALSE
    )
  }

  documento_xml <- tryCatch(
    xml2::read_xml(ruta_xml),
    error = function(e) {
      stop(
        paste0(
          "No se pudo leer la estructura XML del documento Word: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  cuerpo <- xml2::xml_find_first(
    documento_xml,
    ".//*[local-name()='body']"
  )

  if (inherits(cuerpo, "xml_missing")) {
    stop(
      "No se encontró el cuerpo principal del documento Word.",
      call. = FALSE
    )
  }

  elementos <- xml2::xml_children(cuerpo)

  tablas <- list()
  parrafos <- character(0)
  contador_tablas <- 0L
  ultimo_parrafo <- ""

  for (elemento in elementos) {
    tipo <- xml2::xml_name(elemento)

    if (identical(tipo, "p")) {
      texto_parrafo <- texto_nodo_word(elemento)

      if (nzchar(texto_parrafo)) {
        parrafos <- c(parrafos, texto_parrafo)
        ultimo_parrafo <- texto_parrafo
      }

      next
    }

    if (!identical(tipo, "tbl")) {
      next
    }

    filas_xml <- xml2::xml_find_all(
      elemento,
      "./*[local-name()='tr']"
    )

    if (length(filas_xml) == 0) {
      next
    }

    filas <- list()
    contador_filas <- 0L

    for (fila_xml in filas_xml) {
      celdas_xml <- xml2::xml_find_all(
        fila_xml,
        "./*[local-name()='tc']"
      )

      if (length(celdas_xml) == 0) {
        next
      }

      textos_celdas <- vapply(
        celdas_xml,
        texto_nodo_word,
        character(1)
      )

      contador_filas <- contador_filas + 1L
      filas[[contador_filas]] <- textos_celdas
    }

    if (length(filas) == 0) {
      next
    }

    contador_tablas <- contador_tablas + 1L
    tablas[[contador_tablas]] <- list(
      categoria = if (nzchar(ultimo_parrafo)) {
        ultimo_parrafo
      } else {
        "Sin categoría"
      },
      filas = filas
    )
  }

  list(
    tablas = tablas,
    parrafos = unique(parrafos)
  )
}

detectar_tipo_documento_word <- function(estructura) {
  texto_documento <- normalizar_clave(
    paste(estructura$parrafos, collapse = " ")
  )

  if (
    grepl("listado de compras", texto_documento, fixed = TRUE) ||
    grepl("lista de compras", texto_documento, fixed = TRUE)
  ) {
    return("compra")
  }

  for (tabla in estructura$tablas) {
    if (length(tabla$filas) == 0) next

    encabezado <- tabla$filas[[1]]
    tercera_columna <- if (length(encabezado) >= 3) {
      normalizar_clave(encabezado[[3]])
    } else {
      ""
    }

    if (grepl("precio", tercera_columna, fixed = TRUE)) {
      return("compra")
    }
  }

  "inventario"
}

extraer_registros_inventario_word <- function(archivo_docx) {
  verificar_paquete_word()

  if (!file.exists(archivo_docx)) {
    stop("No se encontró el archivo Word seleccionado.", call. = FALSE)
  }

  extension <- tolower(tools::file_ext(archivo_docx))
  if (!identical(extension, "docx")) {
    stop("El archivo debe tener extensión .docx.", call. = FALSE)
  }

  estructura <- extraer_tablas_word_xml(archivo_docx)
  tablas <- estructura$tablas
  tipo_documento <- detectar_tipo_documento_word(estructura)

  if (length(tablas) == 0) {
    stop(
      paste0(
        "No se encontraron tablas dentro del documento Word. ",
        "El archivo debe contener tablas reales de Word."
      ),
      call. = FALSE
    )
  }

  registros <- list()
  contador <- 0L

  for (tabla in tablas) {
    categoria <- limpiar_texto(tabla$categoria)

    if (length(tabla$filas) == 0) {
      next
    }

    encabezado <- tabla$filas[[1]]
    tercera_columna_encabezado <- if (length(encabezado) >= 3) {
      normalizar_clave(encabezado[[3]])
    } else {
      ""
    }

    tabla_es_compra <- (
      identical(tipo_documento, "compra") ||
        grepl("precio", tercera_columna_encabezado, fixed = TRUE)
    )

    for (fila in tabla$filas) {
      cantidad_texto <- if (length(fila) >= 1) {
        limpiar_texto(fila[[1]])
      } else {
        ""
      }

      elemento <- if (length(fila) >= 2) {
        limpiar_texto(fila[[2]])
      } else {
        ""
      }

      tercera_columna <- if (length(fila) >= 3) {
        limpiar_texto(
          paste(fila[3:length(fila)], collapse = " ")
        )
      } else {
        ""
      }

      cantidad_clave <- normalizar_clave(cantidad_texto)
      elemento_clave <- normalizar_clave(elemento)

      es_encabezado <- (
        cantidad_clave %in% c("cantidad", "cant.") &&
          elemento_clave %in% c(
            "elemento",
            "material",
            "producto"
          )
      )

      if (es_encabezado || !nzchar(elemento)) {
        next
      }

      if (tabla_es_compra) {
        contador <- contador + 1L

        registros[[contador]] <- data.frame(
          categoria = categoria,
          elemento = elemento,
          cantidad_original = cantidad_texto,
          cantidad = 0,
          unidad = "unidad",
          contenido_por_unidad = NA_real_,
          detalle_stock = "Pendiente de compra",
          observaciones = "",
          es_compra = 1L,
          cantidad_compra = cantidad_texto,
          precio_compra = tercera_columna,
          tipo_documento = "compra",
          stringsAsFactors = FALSE
        )

        next
      }

      cantidad <- interpretar_cantidad_word(cantidad_texto)
      detalle <- cantidad$detalle

      if (
        !is.na(cantidad$cantidad) &&
        nzchar(cantidad_texto) &&
        is.na(cantidad$contenido_por_unidad) &&
        normalizar_clave(cantidad$unidad) == "unidad"
      ) {
        solo_numero <- grepl(
          "^(aprox\\.?\\s*)?[0-9]+(?:[\\.,][0-9]+)?$",
          cantidad_clave,
          perl = TRUE
        )

        if (
          solo_numero &&
          !grepl("^aprox", cantidad_clave)
        ) {
          detalle <- ""
        }
      }

      contador <- contador + 1L
      registros[[contador]] <- data.frame(
        categoria = categoria,
        elemento = elemento,
        cantidad_original = cantidad_texto,
        cantidad = cantidad$cantidad,
        unidad = cantidad$unidad,
        contenido_por_unidad = cantidad$contenido_por_unidad,
        detalle_stock = detalle,
        observaciones = tercera_columna,
        es_compra = 0L,
        cantidad_compra = "",
        precio_compra = "",
        tipo_documento = "inventario",
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(registros) == 0) {
    stop(
      paste0(
        "Se encontraron tablas, pero no filas importables. ",
        "Las tablas deben tener las columnas Cantidad y Elemento."
      ),
      call. = FALSE
    )
  }

  resultado <- do.call(rbind, registros)
  rownames(resultado) <- NULL
  resultado
}

importar_inventario_docx <- function(
  ruta_db = NULL,
  archivo_docx,
  crear_backup_previo = TRUE
) {
  registros <- extraer_registros_inventario_word(archivo_docx)

  backup <- NULL
  if (isTRUE(crear_backup_previo)) {
    backup <- crear_backup(ruta_db)
  }

  pool_datos <- obtener_pool_datos()

  resultado <- pool::poolWithTransaction(pool_datos, function(con) {
    importados <- 0L
    actualizados_compra <- 0L
    omitidos <- 0L
    categorias_nuevas <- 0L
    omitidos_detalle <- character(0)

    for (i in seq_len(nrow(registros))) {
      registro <- registros[i, , drop = FALSE]
      categoria_nombre <- limpiar_texto(registro$categoria[[1]])
      elemento <- limpiar_texto(registro$elemento[[1]])
      es_compra <- as.integer(registro$es_compra[[1]]) == 1L

      if (!nzchar(categoria_nombre)) categoria_nombre <- "Sin categoría"

      categoria_existente <- DBI::dbGetQuery(
        con,
        "SELECT id FROM categorias WHERE LOWER(nombre) = LOWER($1) LIMIT 1;",
        params = list(categoria_nombre)
      )

      if (nrow(categoria_existente) == 0) {
        orden <- DBI::dbGetQuery(
          con,
          "SELECT COALESCE(MAX(orden), 0) + 1 AS siguiente FROM categorias;"
        )$siguiente[[1]]

        categoria_insertada <- DBI::dbGetQuery(
          con,
          paste0(
            "INSERT INTO categorias(nombre, orden, creado_en) ",
            "VALUES ($1, $2, NOW()) ON CONFLICT DO NOTHING RETURNING id;"
          ),
          params = list(categoria_nombre, as.integer(orden))
        )

        if (nrow(categoria_insertada) == 0) {
          categoria_insertada <- DBI::dbGetQuery(
            con,
            "SELECT id FROM categorias WHERE LOWER(nombre) = LOWER($1) LIMIT 1;",
            params = list(categoria_nombre)
          )
        } else {
          categorias_nuevas <- categorias_nuevas + 1L
        }

        categoria_id <- as.integer(categoria_insertada$id[[1]])
      } else {
        categoria_id <- as.integer(categoria_existente$id[[1]])
      }

      duplicado <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT id FROM materiales ",
          "WHERE LOWER(nombre) = LOWER($1) AND categoria_id = $2 LIMIT 1;"
        ),
        params = list(elemento, categoria_id)
      )

      if (nrow(duplicado) > 0) {
        if (es_compra) {
          DBI::dbExecute(
            con,
            paste0(
              "UPDATE materiales SET ",
              "es_compra = TRUE, cantidad_compra = $1, precio_compra = $2, ",
              "origen = 'word_compra', actualizado_en = NOW() ",
              "WHERE id = $3;"
            ),
            params = list(
              limpiar_texto(registro$cantidad_compra[[1]]),
              limpiar_texto(registro$precio_compra[[1]]),
              as.integer(duplicado$id[[1]])
            )
          )

          actualizados_compra <- actualizados_compra + 1L
        } else {
          omitidos <- omitidos + 1L
          omitidos_detalle <- c(
            omitidos_detalle,
            paste0(categoria_nombre, " / ", elemento)
          )
        }

        next
      }

      origen <- if (es_compra) "word_compra" else "word_inventario"

      material_nuevo <- DBI::dbGetQuery(
        con,
        paste0(
          "INSERT INTO materiales(",
          "nombre, categoria_id, comentarios, observaciones, ",
          "es_compra, cantidad_compra, precio_compra, origen, ",
          "creado_en, actualizado_en",
          ") VALUES ($1, $2, '', $3, $4, $5, $6, $7, NOW(), NOW()) ",
          "RETURNING id;"
        ),
        params = list(
          elemento,
          categoria_id,
          limpiar_texto(registro$observaciones[[1]]),
          es_compra,
          limpiar_texto(registro$cantidad_compra[[1]]),
          limpiar_texto(registro$precio_compra[[1]]),
          origen
        )
      )

      material_id <- as.integer(material_nuevo$id[[1]])

      DBI::dbExecute(
        con,
        paste0(
          "INSERT INTO stock(",
          "material_id, cantidad, unidad, contenido_por_unidad, detalle, orden, ",
          "creado_en, actualizado_en",
          ") VALUES ($1, $2, $3, $4, $5, 1, NOW(), NOW());"
        ),
        params = list(
          material_id,
          numero_o_na(registro$cantidad[[1]]),
          limpiar_texto(registro$unidad[[1]]),
          numero_o_na(registro$contenido_por_unidad[[1]]),
          limpiar_texto(registro$detalle_stock[[1]])
        )
      )

      importados <- importados + 1L
    }

    cambios <- importados + actualizados_compra + categorias_nuevas

    if (cambios > 0) {
      registrar_cambio(
        con,
        accion = "importar_word",
        entidad = "documento",
        entidad_id = NA_integer_,
        detalle = paste0(
          "tipo=",
          if (all(registros$es_compra == 1L)) "compra" else "inventario",
          "; detectados=", nrow(registros),
          "; importados=", importados,
          "; compras_actualizadas=", actualizados_compra,
          "; omitidos=", omitidos,
          "; categorias_nuevas=", categorias_nuevas
        )
      )
    }

    list(
      total_detectado = nrow(registros),
      importados = importados,
      actualizados_compra = actualizados_compra,
      omitidos = omitidos,
      categorias_nuevas = categorias_nuevas,
      compras_detectadas = sum(registros$es_compra == 1L),
      tipo_documento = if (all(registros$es_compra == 1L)) {
        "compra"
      } else {
        "inventario"
      },
      omitidos_detalle = unique(omitidos_detalle)
    )
  })

  resultado$backup <- backup
  resultado$registros <- registros
  resultado
}