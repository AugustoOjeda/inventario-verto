if (!requireNamespace("shiny", quietly = TRUE)) {
  stop(
    'Falta el paquete "shiny". Instalalo con: install.packages("shiny")',
    call. = FALSE
  )
}

library(shiny)

# ------------------------------------------------------------
# Detectar la carpeta donde se encuentra main.R
# ------------------------------------------------------------

obtener_directorio_programa <- function() {
  argumentos <- commandArgs(trailingOnly = FALSE)
  argumento_archivo <- grep("^--file=", argumentos, value = TRUE)

  if (length(argumento_archivo) > 0) {
    ruta_archivo <- sub("^--file=", "", argumento_archivo[[1]])
    return(dirname(normalizePath(
      ruta_archivo,
      winslash = "/",
      mustWork = FALSE
    )))
  }

  archivos_origen <- vapply(
    sys.frames(),
    function(x) {
      if (!is.null(x$ofile)) as.character(x$ofile) else NA_character_
    },
    character(1)
  )

  archivos_origen <- archivos_origen[!is.na(archivos_origen)]

  if (length(archivos_origen) > 0) {
    return(dirname(normalizePath(
      tail(archivos_origen, 1),
      winslash = "/",
      mustWork = FALSE
    )))
  }

  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

DIRECTORIO_PROGRAMA <- obtener_directorio_programa()
ARCHIVO_FUNCIONES <- file.path(DIRECTORIO_PROGRAMA, "function.R")

if (!file.exists(ARCHIVO_FUNCIONES)) {
  stop(
    paste0(
      "No se encontró function.R en la carpeta:\n",
      DIRECTORIO_PROGRAMA
    ),
    call. = FALSE
  )
}

source(ARCHIVO_FUNCIONES, local = TRUE)

# El parámetro se conserva por compatibilidad con las funciones.
RUTAS <- crear_rutas_programa(DIRECTORIO_PROGRAMA)

# La conexión, las tablas y las categorías iniciales se preparan al iniciar.
inicializar_base_datos(RUTAS$base_datos)

options(
  shiny.maxRequestSize = 30 * 1024^2,
  shiny.fullstacktrace = FALSE
)

# ------------------------------------------------------------
# Configuración general
# ------------------------------------------------------------

CLAVE_APLICACION <- Sys.getenv("APP_PASSWORD", unset = "")
REQUIERE_CLAVE <- nzchar(CLAVE_APLICACION)

# ------------------------------------------------------------
# Funciones auxiliares de interfaz
# ------------------------------------------------------------

leer_numero <- function(valor, nombre_campo, permitir_vacio = TRUE) {
  valor <- limpiar_texto(valor)

  if (!nzchar(valor)) {
    if (permitir_vacio) return(NA_real_)
    stop(paste0("Completá el campo ", nombre_campo, "."), call. = FALSE)
  }

  valor <- gsub(",", ".", valor, fixed = TRUE)
  numero <- suppressWarnings(as.numeric(valor))

  if (is.na(numero)) {
    stop(
      paste0("El campo ", nombre_campo, " debe contener un número válido."),
      call. = FALSE
    )
  }

  numero
}

numero_para_input <- function(valor) {
  if (is.null(valor) || length(valor) == 0 || is.na(valor)) return("")
  format(valor, scientific = FALSE, trim = TRUE)
}

texto_seguro <- function(valor, reemplazo = "") {
  valor <- limpiar_texto(valor)
  if (nzchar(valor)) valor else reemplazo
}

boton_evento <- function(
  texto,
  input_id,
  datos_js,
  clase = "btn-control",
  titulo = NULL
) {
  tags$button(
    type = "button",
    class = clase,
    title = titulo,
    onclick = sprintf(
      "Shiny.setInputValue('%s', %s, {priority: 'event'});",
      input_id,
      datos_js
    ),
    texto
  )
}

crear_choices_stock <- function(stock) {
  choices <- c("Crear una presentación nueva" = "")

  if (nrow(stock) == 0) return(choices)

  etiquetas <- vapply(
    seq_len(nrow(stock)),
    function(i) {
      cantidad <- formatear_cantidad(
        stock$cantidad[[i]],
        stock$unidad[[i]],
        stock$contenido_por_unidad[[i]]
      )

      detalle <- limpiar_texto(stock$detalle[[i]])

      if (nzchar(detalle)) {
        paste0(cantidad, " — ", detalle)
      } else {
        cantidad
      }
    },
    character(1)
  )

  c(choices, setNames(as.character(stock$id), etiquetas))
}

# ------------------------------------------------------------
# Estilos
# ------------------------------------------------------------

estilos_css <- "
:root {
  --fondo: #20242b;
  --fondo-secundario: #292e36;
  --panel: #313740;
  --panel-claro: #383f49;
  --borde: #4a535f;
  --texto: #edf2f7;
  --texto-secundario: #b8c1cc;
  --celeste: #68c7e8;
  --celeste-oscuro: #2d91b5;
  --rojo: #d56b6b;
  --rojo-fondo: #442d31;
  --rojo-borde: #9a5159;
  --verde: #73be91;
  --verde-fondo: #243c31;
  --amarillo: #d2b868;
  --sombra: rgba(0, 0, 0, 0.22);
}

html,
body {
  min-height: 100%;
  background: var(--fondo);
  color: var(--texto);
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif;
}

body {
  margin: 0;
}

.inventario-header {
  width: 100%;
  background: #171b21;
  border-bottom: 1px solid #353c45;
  padding: 17px 26px;
  box-sizing: border-box;
  box-shadow: 0 2px 10px var(--sombra);
}

.header-contenido {
  width: min(1500px, 100%);
  margin: 0 auto;
  display: flex;
  justify-content: space-between;
  align-items: center;
  gap: 16px;
}

.inventario-header h1 {
  color: var(--celeste);
  font-size: 26px;
  font-weight: 700;
  letter-spacing: 0.7px;
  margin: 0;
}

.inventario-header p {
  color: var(--texto-secundario);
  font-size: 13px;
  margin: 5px 0 0 0;
}

.estado-conexion {
  display: inline-flex;
  align-items: center;
  gap: 7px;
  color: var(--texto-secundario);
  background: #242a31;
  border: 1px solid #424b56;
  border-radius: 999px;
  padding: 7px 11px;
  font-size: 11px;
  white-space: nowrap;
}

.estado-punto {
  width: 8px;
  height: 8px;
  border-radius: 50%;
  display: inline-block;
}

.estado-punto-conectado {
  background: var(--verde);
  box-shadow: 0 0 7px rgba(115, 190, 145, 0.55);
}

.estado-punto-error {
  background: var(--rojo);
  box-shadow: 0 0 7px rgba(213, 107, 107, 0.55);
}

.contenedor-principal {
  width: min(1500px, calc(100% - 32px));
  margin: 22px auto 48px auto;
}

.barra-herramientas {
  display: grid;
  grid-template-columns:
    minmax(220px, 1.2fr)
    minmax(220px, 1fr)
    minmax(170px, 0.75fr)
    repeat(4, auto);
  gap: 11px;
  align-items: end;
  background: var(--fondo-secundario);
  border: 1px solid var(--borde);
  border-radius: 8px;
  padding: 15px;
  margin-bottom: 20px;
  box-shadow: 0 4px 14px var(--sombra);
}

.form-group {
  margin-bottom: 0;
}

.control-label {
  color: var(--texto-secundario);
  font-size: 12px;
  font-weight: 600;
  margin-bottom: 6px;
}

.form-control,
.selectize-input,
.selectize-control.single .selectize-input.input-active {
  color: var(--texto);
  background: #22272e;
  border: 1px solid var(--borde);
  border-radius: 5px;
  box-shadow: none;
}

.form-control:focus,
.selectize-input.focus {
  border-color: var(--celeste);
  box-shadow: 0 0 0 2px rgba(104, 199, 232, 0.15);
}

.selectize-dropdown,
.selectize-dropdown-content {
  color: var(--texto);
  background: #22272e;
  border-color: var(--borde);
}

.selectize-dropdown .active {
  color: white;
  background: var(--celeste-oscuro);
}

.btn {
  border-radius: 5px;
  font-weight: 600;
  transition: 0.15s ease;
}

.btn:hover {
  transform: translateY(-1px);
}

.btn-principal {
  color: #10212a;
  background: var(--celeste);
  border: 1px solid var(--celeste);
}

.btn-principal:hover,
.btn-principal:focus {
  color: #0b1920;
  background: #82d5f0;
  border-color: #82d5f0;
}

.btn-secundario {
  color: var(--texto);
  background: var(--panel-claro);
  border: 1px solid var(--borde);
}

.btn-secundario:hover,
.btn-secundario:focus {
  color: white;
  background: #454e5a;
  border-color: #5a6573;
}

.btn-peligro {
  color: white;
  background: #8e4444;
  border: 1px solid #a95656;
}

.btn-peligro:hover,
.btn-peligro:focus {
  color: white;
  background: #a14c4c;
  border-color: #bd6262;
}

.categoria-bloque {
  margin-bottom: 22px;
}

.categoria-titulo {
  display: flex;
  align-items: center;
  gap: 10px;
  color: var(--celeste);
  font-size: 19px;
  font-weight: 700;
  padding: 0 2px 9px 2px;
  border-bottom: 1px solid var(--borde);
  margin-bottom: 11px;
}

.categoria-contador {
  color: var(--texto-secundario);
  background: #272d35;
  border: 1px solid #424b56;
  border-radius: 999px;
  font-size: 11px;
  font-weight: 600;
  padding: 3px 8px;
}

.material-card {
  display: grid;
  grid-template-columns: minmax(240px, 1.25fr) minmax(310px, 1.8fr) auto;
  gap: 18px;
  align-items: center;
  background: var(--panel);
  border: 1px solid var(--borde);
  border-radius: 7px;
  padding: 14px 15px;
  margin-bottom: 9px;
  box-shadow: 0 3px 10px var(--sombra);
}

.material-card-comprar {
  background:
    linear-gradient(
      90deg,
      rgba(213, 107, 107, 0.13),
      rgba(68, 45, 49, 0.45)
    ),
    var(--panel);
  border-color: var(--rojo-borde);
  box-shadow:
    inset 4px 0 0 var(--rojo),
    0 3px 10px var(--sombra);
}

.material-encabezado {
  display: flex;
  align-items: flex-start;
  gap: 8px;
  flex-wrap: wrap;
}

.material-nombre {
  color: var(--texto);
  font-size: 16px;
  font-weight: 700;
  margin-bottom: 5px;
  overflow-wrap: anywhere;
}

.badge-comprar {
  display: inline-flex;
  align-items: center;
  color: #ffe1e1;
  background: #7d3f47;
  border: 1px solid #a95a64;
  border-radius: 999px;
  font-size: 10px;
  font-weight: 800;
  letter-spacing: 0.5px;
  padding: 4px 8px;
  line-height: 1;
}

.material-texto-secundario {
  color: var(--texto-secundario);
  font-size: 12px;
  line-height: 1.45;
  margin-top: 3px;
  overflow-wrap: anywhere;
}

.etiqueta {
  color: #d7dee7;
  font-weight: 700;
}

.compra-panel {
  color: #f1d5d7;
  background: rgba(91, 48, 54, 0.58);
  border: 1px solid rgba(169, 90, 100, 0.72);
  border-radius: 5px;
  padding: 8px 10px;
  margin-top: 8px;
  font-size: 12px;
  line-height: 1.45;
}

.compra-titulo {
  color: #ffd3d6;
  font-weight: 800;
  letter-spacing: 0.3px;
  margin-bottom: 3px;
}

.stock-contenedor {
  display: flex;
  flex-direction: column;
  gap: 7px;
}

.stock-fila {
  display: grid;
  grid-template-columns: 34px minmax(125px, auto) 34px minmax(90px, 1fr) 32px 32px;
  gap: 6px;
  align-items: center;
  background: #272d34;
  border: 1px solid #444d58;
  border-radius: 5px;
  padding: 6px;
}

.stock-fila-sin-disponibilidad {
  border-color: #785057;
  background: #33282d;
}

.stock-cantidad {
  color: white;
  font-size: 14px;
  font-weight: 700;
  text-align: center;
  white-space: nowrap;
}

.stock-detalle {
  color: var(--texto-secundario);
  font-size: 11px;
  overflow-wrap: anywhere;
}

.btn-control {
  width: 34px;
  height: 30px;
  color: var(--texto);
  background: #3b434d;
  border: 1px solid #586371;
  border-radius: 4px;
  font-size: 12px;
  line-height: 1;
  cursor: pointer;
}

.btn-control:hover {
  color: #10212a;
  background: var(--celeste);
  border-color: var(--celeste);
}

.btn-control-editar {
  width: 32px;
  height: 30px;
  color: var(--texto);
  background: #3b434d;
  border: 1px solid #586371;
  border-radius: 4px;
  cursor: pointer;
}

.btn-control-eliminar {
  width: 32px;
  height: 30px;
  color: #ffdada;
  background: #553737;
  border: 1px solid #785050;
  border-radius: 4px;
  cursor: pointer;
}

.material-acciones {
  display: flex;
  flex-wrap: wrap;
  justify-content: flex-end;
  gap: 7px;
  min-width: 220px;
}

.btn-accion {
  color: var(--texto);
  background: #3b434d;
  border: 1px solid #586371;
  border-radius: 4px;
  padding: 7px 9px;
  font-size: 11px;
  font-weight: 600;
  cursor: pointer;
}

.btn-accion:hover {
  border-color: var(--celeste);
}

.btn-accion-compra {
  color: #ffe0e0;
  background: #684047;
  border: 1px solid #965761;
  border-radius: 4px;
  padding: 7px 9px;
  font-size: 11px;
  font-weight: 700;
  cursor: pointer;
}

.btn-accion-compra:hover {
  background: #7a4851;
  border-color: #b46772;
}

.btn-accion-recibido {
  color: #dff7e8;
  background: #31503f;
  border: 1px solid #4f7b63;
  border-radius: 4px;
  padding: 7px 9px;
  font-size: 11px;
  font-weight: 700;
  cursor: pointer;
}

.btn-accion-recibido:hover {
  background: #3b604c;
  border-color: #639879;
}

.btn-accion-eliminar {
  color: #ffd8d8;
  background: #563737;
  border: 1px solid #7d5050;
  border-radius: 4px;
  padding: 7px 9px;
  font-size: 11px;
  font-weight: 600;
  cursor: pointer;
}

.estado-vacio {
  color: var(--texto-secundario);
  text-align: center;
  background: var(--fondo-secundario);
  border: 1px dashed var(--borde);
  border-radius: 8px;
  padding: 48px 20px;
}

.pie-aplicacion {
  display: flex;
  justify-content: space-between;
  gap: 16px;
  color: #8e99a6;
  font-size: 11px;
  margin-top: 18px;
}

.modal-content {
  color: var(--texto);
  background: var(--fondo-secundario);
  border: 1px solid var(--borde);
  border-radius: 8px;
}

.modal-header,
.modal-footer {
  border-color: var(--borde);
}

.modal-title {
  color: var(--celeste);
  font-weight: 700;
}

.close {
  color: white;
  opacity: 0.8;
  text-shadow: none;
}

.close:hover {
  color: white;
  opacity: 1;
}

.modal-body .form-group {
  margin-bottom: 15px;
}

.fila-formulario {
  display: grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  gap: 12px;
}

.fila-formulario-dos {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 12px;
}

.categoria-gestion-fila {
  display: flex;
  justify-content: space-between;
  align-items: center;
  gap: 12px;
  background: #252b32;
  border: 1px solid #414a55;
  border-radius: 5px;
  padding: 8px 10px;
  margin-bottom: 6px;
}

.categoria-gestion-nombre {
  overflow-wrap: anywhere;
}

.categoria-gestion-eliminar {
  color: #ffd8d8;
  background: #563737;
  border: 1px solid #7d5050;
  border-radius: 4px;
  padding: 5px 8px;
  cursor: pointer;
}

.separador-modal {
  border-top: 1px solid var(--borde);
  margin: 18px 0;
}

.login-pagina {
  min-height: 100vh;
  display: grid;
  place-items: center;
  padding: 22px;
  box-sizing: border-box;
  background:
    radial-gradient(circle at top, #2a313a 0, #20242b 42%, #1b1f25 100%);
}

.login-panel {
  width: min(420px, 100%);
  background: #292e36;
  border: 1px solid #4a535f;
  border-radius: 10px;
  padding: 28px;
  box-sizing: border-box;
  box-shadow: 0 12px 35px rgba(0, 0, 0, 0.35);
}

.login-panel h1 {
  color: var(--celeste);
  font-size: 24px;
  margin: 0 0 7px 0;
}

.login-panel p {
  color: var(--texto-secundario);
  margin: 0 0 20px 0;
  line-height: 1.5;
}

.login-error {
  color: #ffd5d5;
  background: #5f363b;
  border: 1px solid #8f5058;
  border-radius: 5px;
  padding: 8px 10px;
  margin-top: 12px;
  font-size: 12px;
}

.aviso-sin-clave {
  color: #e8d9a8;
  background: #4a4229;
  border: 1px solid #75693d;
  border-radius: 5px;
  padding: 8px 10px;
  font-size: 11px;
}

.shiny-notification {
  color: var(--texto);
  background: #252b32;
  border: 1px solid var(--borde);
  box-shadow: 0 5px 18px var(--sombra);
}

.progress {
  background: #20252c;
}

.progress-bar {
  background: var(--celeste-oscuro);
}

@media (max-width: 1250px) {
  .barra-herramientas {
    grid-template-columns: repeat(3, minmax(190px, 1fr));
  }

  .material-card {
    grid-template-columns: 1fr;
  }

  .material-acciones {
    justify-content: flex-start;
    min-width: 0;
  }
}

@media (max-width: 760px) {
  .contenedor-principal {
    width: min(100% - 18px, 1500px);
    margin-top: 12px;
  }

  .inventario-header {
    padding: 15px 16px;
  }

  .header-contenido {
    align-items: flex-start;
    flex-direction: column;
  }

  .inventario-header h1 {
    font-size: 21px;
  }

  .barra-herramientas {
    grid-template-columns: 1fr;
  }

  .fila-formulario,
  .fila-formulario-dos {
    grid-template-columns: 1fr;
  }

  .stock-fila {
    grid-template-columns: 32px minmax(108px, auto) 32px 1fr 30px 30px;
  }

  .pie-aplicacion {
    flex-direction: column;
  }
}
"

# ------------------------------------------------------------
# Componentes principales de UI
# ------------------------------------------------------------

ui_login <- function() {
  div(
    class = "login-pagina",
    div(
      class = "login-panel",
      tags$h1("INVENTARIO VERTO"),
      tags$p(
        "Ingresá la clave compartida para acceder al control de stock."
      ),
      passwordInput(
        "clave_acceso",
        "Clave de acceso",
        placeholder = "Ingresá la clave"
      ),
      actionButton(
        "ingresar_aplicacion",
        "Entrar",
        class = "btn-principal",
        width = "100%"
      ),
      uiOutput("login_error_ui")
    )
  )
}

ui_aplicacion <- function() {
  tagList(
    tags$header(
      class = "inventario-header",
      div(
        class = "header-contenido",
        div(
          tags$h1("INVENTARIO VERTO"),
          tags$p("Control centralizado de materiales, equipamiento y reactivos")
        ),
        uiOutput("estado_conexion_ui")
      )
    ),

    div(
      class = "contenedor-principal",

      div(
        class = "barra-herramientas",

        textInput(
          "buscar_material",
          "Buscar material",
          placeholder = "Nombre, comentario u observación"
        ),

        selectInput(
          "filtro_categoria",
          "Categoría",
          choices = c("Todas las categorías" = "todas"),
          selected = "todas"
        ),

        selectInput(
          "filtro_estado",
          "Estado",
          choices = c(
            "Todos los materiales" = "todos",
            "Solo para comprar" = "comprar",
            "Solo con stock" = "disponible"
          ),
          selected = "todos"
        ),

        actionButton(
          "abrir_nuevo_material",
          "Añadir material",
          class = "btn-principal"
        ),

        actionButton(
          "abrir_categorias",
          "Categorías",
          class = "btn-secundario"
        ),

        actionButton(
          "abrir_importar_word",
          "Importar Word",
          class = "btn-secundario"
        ),

        downloadButton(
          "descargar_backup",
          "Descargar backup",
          class = "btn-secundario"
        )
      ),

      uiOutput("inventario_ui"),

      div(
        class = "pie-aplicacion",
        span("Los cambios se sincronizan automáticamente entre usuarios."),
        span("Base de datos: Neon PostgreSQL")
      )
    )
  )
}

# ------------------------------------------------------------
# UI general
# ------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$title("Inventario Verto"),
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    tags$style(HTML(estilos_css))
  ),
  uiOutput("pagina_ui")
)

# ------------------------------------------------------------
# Servidor
# ------------------------------------------------------------

server <- function(input, output, session) {
  autenticado <- reactiveVal(!REQUIERE_CLAVE)
  error_login <- reactiveVal("")

  version_local <- reactiveVal(0L)

  material_seleccionado <- reactiveVal(NULL)
  stock_seleccionado <- reactiveVal(NULL)
  material_a_eliminar <- reactiveVal(NULL)
  stock_a_eliminar <- reactiveVal(NULL)
  material_compra_seleccionado <- reactiveVal(NULL)

  refrescar <- function() {
    version_local(version_local() + 1L)
  }

  ejecutar_accion <- function(
    accion,
    mensaje_exito = NULL,
    cerrar_modal = FALSE
  ) {
    tryCatch(
      {
        resultado <- accion()

        if (cerrar_modal) {
          removeModal()
        }

        refrescar()

        if (!is.null(mensaje_exito) && nzchar(mensaje_exito)) {
          showNotification(
            mensaje_exito,
            type = "message",
            duration = 4
          )
        }

        invisible(resultado)
      },
      error = function(e) {
        showNotification(
          conditionMessage(e),
          type = "error",
          duration = 9
        )
        invisible(NULL)
      }
    )
  }

  # ----------------------------------------------------------
  # Acceso mediante clave compartida
  # ----------------------------------------------------------

  output$pagina_ui <- renderUI({
    if (isTRUE(autenticado())) {
      ui_aplicacion()
    } else {
      ui_login()
    }
  })

  output$login_error_ui <- renderUI({
    mensaje <- error_login()
    if (!nzchar(mensaje)) return(NULL)

    div(class = "login-error", mensaje)
  })

  observeEvent(input$ingresar_aplicacion, {
    clave_ingresada <- isolate(input$clave_acceso)

    if (identical(clave_ingresada, CLAVE_APLICACION)) {
      error_login("")
      autenticado(TRUE)
    } else {
      error_login("La clave ingresada no es correcta.")
      updateTextInput(session, "clave_acceso", value = "")
    }
  })

  observeEvent(input$clave_acceso, {
    if (nzchar(error_login())) error_login("")
  }, ignoreInit = TRUE)

  # ----------------------------------------------------------
  # Estado de conexión y sincronización remota
  # ----------------------------------------------------------

  revision_remota <- reactivePoll(
    intervalMillis = 3500,
    session = session,
    checkFunc = function() {
      tryCatch(
        obtener_revision(RUTAS$base_datos),
        error = function(e) paste0("error-", as.numeric(Sys.time()))
      )
    },
    valueFunc = function() {
      tryCatch(
        obtener_revision(RUTAS$base_datos),
        error = function(e) NA_integer_
      )
    }
  )

  output$estado_conexion_ui <- renderUI({
    invalidateLater(12000, session)

    estado <- tryCatch(
      probar_conexion_datos(),
      error = function(e) {
        list(conectado = FALSE, mensaje = conditionMessage(e))
      }
    )

    if (isTRUE(estado$conectado)) {
      div(
        class = "estado-conexion",
        span(class = "estado-punto estado-punto-conectado"),
        span("Conectado")
      )
    } else {
      div(
        class = "estado-conexion",
        title = texto_seguro(estado$mensaje, "Error de conexión"),
        span(class = "estado-punto estado-punto-error"),
        span("Reconectando")
      )
    }
  })

  # ----------------------------------------------------------
  # Categorías y filtros
  # ----------------------------------------------------------

  obtener_choices_categorias <- function() {
    categorias <- obtener_categorias(RUTAS$base_datos)

    setNames(
      as.character(categorias$id),
      categorias$nombre
    )
  }

  actualizar_filtro_categorias <- function() {
    categorias <- obtener_categorias(RUTAS$base_datos)
    valores <- as.character(categorias$id)

    seleccionado <- isolate(input$filtro_categoria)

    if (
      is.null(seleccionado) ||
      !seleccionado %in% c("todas", valores)
    ) {
      seleccionado <- "todas"
    }

    updateSelectInput(
      session,
      "filtro_categoria",
      choices = c(
        "Todas las categorías" = "todas",
        setNames(valores, categorias$nombre)
      ),
      selected = seleccionado
    )
  }

  observe({
    req(autenticado())
    revision_remota()
    version_local()

    try(
      actualizar_filtro_categorias(),
      silent = TRUE
    )
  })

  # ----------------------------------------------------------
  # Render principal del inventario
  # ----------------------------------------------------------

  output$inventario_ui <- renderUI({
    req(autenticado())

    revision_remota()
    version_local()

    datos <- tryCatch(
      obtener_inventario(RUTAS$base_datos),
      error = function(e) {
        showNotification(
          paste0("No se pudo leer el inventario: ", conditionMessage(e)),
          type = "error",
          duration = 8
        )
        NULL
      }
    )

    if (is.null(datos)) {
      return(
        div(
          class = "estado-vacio",
          tags$h3("No se pudo conectar con el inventario"),
          tags$p("La aplicación volverá a intentarlo automáticamente.")
        )
      )
    }

    if (nrow(datos) == 0) {
      return(
        div(
          class = "estado-vacio",
          tags$h3("Todavía no hay materiales cargados"),
          tags$p("Usá “Añadir material” o “Importar Word”.")
        )
      )
    }

    categoria_filtrada <- input$filtro_categoria
    estado_filtrado <- input$filtro_estado
    busqueda <- tolower(limpiar_texto(input$buscar_material))

    if (
      !is.null(categoria_filtrada) &&
      nzchar(categoria_filtrada) &&
      categoria_filtrada != "todas"
    ) {
      datos <- datos[
        as.character(datos$categoria_id) == categoria_filtrada,
        ,
        drop = FALSE
      ]
    }

    if (!is.null(estado_filtrado) && nrow(datos) > 0) {
      if (identical(estado_filtrado, "comprar")) {
        datos <- datos[
          as.logical(datos$requiere_compra),
          ,
          drop = FALSE
        ]
      } else if (identical(estado_filtrado, "disponible")) {
        datos <- datos[
          !as.logical(datos$requiere_compra),
          ,
          drop = FALSE
        ]
      }
    }

    if (nzchar(busqueda) && nrow(datos) > 0) {
      texto_busqueda <- tolower(
        paste(
          datos$material,
          datos$categoria,
          datos$comentarios,
          datos$observaciones,
          datos$detalle_stock,
          datos$cantidad_compra,
          datos$precio_compra
        )
      )

      datos <- datos[
        grepl(busqueda, texto_busqueda, fixed = TRUE),
        ,
        drop = FALSE
      ]
    }

    if (nrow(datos) == 0) {
      return(
        div(
          class = "estado-vacio",
          tags$h3("No se encontraron resultados"),
          tags$p("Probá con otra búsqueda, categoría o estado.")
        )
      )
    }

    categorias <- unique(
      datos[, c(
        "categoria_id",
        "categoria",
        "categoria_orden"
      )]
    )

    categorias <- categorias[
      order(categorias$categoria_orden, categorias$categoria),
      ,
      drop = FALSE
    ]

    bloques_categorias <- lapply(
      seq_len(nrow(categorias)),
      function(indice_categoria) {
        categoria_actual <- categorias[indice_categoria, ]

        datos_categoria <- datos[
          datos$categoria_id == categoria_actual$categoria_id,
          ,
          drop = FALSE
        ]

        materiales_categoria <- unique(
          datos_categoria[, c(
            "material_id",
            "material",
            "comentarios",
            "observaciones",
            "es_compra",
            "cantidad_compra",
            "precio_compra",
            "origen",
            "requiere_compra",
            "stock_sin_disponibilidad"
          )]
        )

        tarjetas_material <- lapply(
          seq_len(nrow(materiales_categoria)),
          function(indice_material) {
            material_actual <- materiales_categoria[indice_material, ]
            material_id <- as.integer(material_actual$material_id)

            filas_material <- datos_categoria[
              datos_categoria$material_id == material_id,
              ,
              drop = FALSE
            ]

            filas_stock <- filas_material[
              !is.na(filas_material$stock_id),
              ,
              drop = FALSE
            ]

            requiere_compra <- isTRUE(
              as.logical(material_actual$requiere_compra[[1]])
            )

            marcado_compra <- isTRUE(
              as.logical(material_actual$es_compra[[1]])
            )

            stock_ui <- if (nrow(filas_stock) == 0) {
              div(
                class = "material-texto-secundario",
                "Este material no tiene presentaciones de stock."
              )
            } else {
              tagList(
                lapply(
                  seq_len(nrow(filas_stock)),
                  function(indice_stock) {
                    stock_actual <- filas_stock[indice_stock, ]
                    stock_id <- as.integer(stock_actual$stock_id)

                    detalle <- limpiar_texto(stock_actual$detalle_stock)
                    cantidad <- stock_actual$cantidad[[1]]

                    cantidad_texto <- formatear_cantidad(
                      cantidad,
                      stock_actual$unidad[[1]],
                      stock_actual$contenido_por_unidad[[1]]
                    )

                    sin_disponibilidad <- (
                      is.na(cantidad) ||
                        (!is.na(cantidad) && cantidad <= 0)
                    )

                    div(
                      class = paste(
                        "stock-fila",
                        if (sin_disponibilidad) {
                          "stock-fila-sin-disponibilidad"
                        } else {
                          ""
                        }
                      ),

                      boton_evento(
                        "▼",
                        "stock_action",
                        sprintf(
                          "{id: %d, delta: -1, nonce: Date.now()}",
                          stock_id
                        ),
                        clase = "btn-control",
                        titulo = "Restar una unidad"
                      ),

                      div(
                        class = "stock-cantidad",
                        cantidad_texto
                      ),

                      boton_evento(
                        "▲",
                        "stock_action",
                        sprintf(
                          "{id: %d, delta: 1, nonce: Date.now()}",
                          stock_id
                        ),
                        clase = "btn-control",
                        titulo = "Sumar una unidad"
                      ),

                      div(
                        class = "stock-detalle",
                        if (nzchar(detalle)) {
                          detalle
                        } else if (sin_disponibilidad) {
                          "Sin disponibilidad"
                        } else {
                          "Sin detalle adicional"
                        }
                      ),

                      boton_evento(
                        "✎",
                        "stock_edit_action",
                        sprintf(
                          "{id: %d, nonce: Date.now()}",
                          stock_id
                        ),
                        clase = "btn-control-editar",
                        titulo = "Editar esta presentación"
                      ),

                      boton_evento(
                        "×",
                        "stock_delete_action",
                        sprintf(
                          "{id: %d, nonce: Date.now()}",
                          stock_id
                        ),
                        clase = "btn-control-eliminar",
                        titulo = "Eliminar esta presentación"
                      )
                    )
                  }
                )
              )
            }

            comentarios <- limpiar_texto(material_actual$comentarios[[1]])
            observaciones <- limpiar_texto(material_actual$observaciones[[1]])
            cantidad_compra <- limpiar_texto(
              material_actual$cantidad_compra[[1]]
            )
            precio_compra <- limpiar_texto(
              material_actual$precio_compra[[1]]
            )

            razon_compra <- if (marcado_compra) {
              "Compra pendiente registrada"
            } else {
              "Stock agotado o cantidad no informada"
            }

            panel_compra <- if (requiere_compra) {
              div(
                class = "compra-panel",
                div(class = "compra-titulo", "COMPRAR"),
                div(razon_compra),
                if (nzchar(cantidad_compra)) {
                  div(
                    span(class = "etiqueta", "Cantidad solicitada: "),
                    cantidad_compra
                  )
                },
                if (nzchar(precio_compra)) {
                  div(
                    span(class = "etiqueta", "Precio de referencia: "),
                    precio_compra
                  )
                }
              )
            }

            acciones_compra <- tagList(
              if (requiere_compra) {
                boton_evento(
                  "Compra realizada",
                  "material_purchase_received_action",
                  sprintf(
                    "{id: %d, nonce: Date.now()}",
                    material_id
                  ),
                  clase = "btn-accion-recibido",
                  titulo = "Registrar el material recibido y sumarlo al stock"
                )
              },
              boton_evento(
                if (marcado_compra) "Editar compra" else "Marcar compra",
                "material_purchase_action",
                sprintf(
                  "{id: %d, nonce: Date.now()}",
                  material_id
                ),
                clase = "btn-accion-compra",
                titulo = "Definir cantidad y precio de compra"
              ),
              if (marcado_compra) {
                boton_evento(
                  "Quitar marca",
                  "material_purchase_clear_action",
                  sprintf(
                    "{id: %d, nonce: Date.now()}",
                    material_id
                  ),
                  clase = "btn-accion",
                  titulo = "Quitar la compra pendiente"
                )
              }
            )

            div(
              class = paste(
                "material-card",
                if (requiere_compra) "material-card-comprar" else ""
              ),

              div(
                div(
                  class = "material-encabezado",
                  div(
                    class = "material-nombre",
                    material_actual$material[[1]]
                  ),
                  if (requiere_compra) {
                    span(class = "badge-comprar", "COMPRAR")
                  }
                ),

                if (nzchar(comentarios)) {
                  div(
                    class = "material-texto-secundario",
                    span(class = "etiqueta", "Comentario: "),
                    comentarios
                  )
                },

                if (nzchar(observaciones)) {
                  div(
                    class = "material-texto-secundario",
                    span(class = "etiqueta", "Observación: "),
                    observaciones
                  )
                },

                panel_compra
              ),

              div(
                class = "stock-contenedor",
                stock_ui
              ),

              div(
                class = "material-acciones",

                acciones_compra,

                boton_evento(
                  "Añadir stock",
                  "material_add_stock_action",
                  sprintf(
                    "{id: %d, nonce: Date.now()}",
                    material_id
                  ),
                  clase = "btn-accion",
                  titulo = "Añadir otra presentación de stock"
                ),

                boton_evento(
                  "Editar",
                  "material_edit_action",
                  sprintf(
                    "{id: %d, nonce: Date.now()}",
                    material_id
                  ),
                  clase = "btn-accion",
                  titulo = "Editar el material"
                ),

                boton_evento(
                  "Eliminar",
                  "material_delete_action",
                  sprintf(
                    "{id: %d, nonce: Date.now()}",
                    material_id
                  ),
                  clase = "btn-accion-eliminar",
                  titulo = "Eliminar el material"
                )
              )
            )
          }
        )

        cantidad_compras <- sum(
          as.logical(materiales_categoria$requiere_compra),
          na.rm = TRUE
        )

        div(
          class = "categoria-bloque",

          div(
            class = "categoria-titulo",
            span(categoria_actual$categoria[[1]]),
            span(
              class = "categoria-contador",
              paste0(
                nrow(materiales_categoria),
                if (nrow(materiales_categoria) == 1) {
                  " material"
                } else {
                  " materiales"
                }
              )
            ),
            if (cantidad_compras > 0) {
              span(
                class = "categoria-contador",
                paste0(cantidad_compras, " para comprar")
              )
            }
          ),

          tagList(tarjetas_material)
        )
      }
    )

    tagList(bloques_categorias)
  })

  # ----------------------------------------------------------
  # Añadir material
  # ----------------------------------------------------------

  observeEvent(input$abrir_nuevo_material, {
    req(autenticado())

    categorias <- obtener_choices_categorias()

    showModal(
      modalDialog(
        title = "Añadir material",
        size = "l",
        easyClose = FALSE,

        textInput(
          "nuevo_nombre",
          "Nombre del material",
          placeholder = "Ejemplo: Guantes descartables, talle M"
        ),

        selectInput(
          "nueva_categoria",
          "Categoría",
          choices = categorias
        ),

        div(
          class = "fila-formulario",

          textInput(
            "nueva_cantidad",
            "Cantidad actual",
            value = "0",
            placeholder = "Vacío para S/C"
          ),

          textInput(
            "nueva_unidad",
            "Unidad",
            value = "unidad",
            placeholder = "unidad, caja, par, kg, mL..."
          ),

          textInput(
            "nuevo_contenido",
            "Contenido por unidad",
            value = "",
            placeholder = "Ejemplo: 100"
          )
        ),

        textInput(
          "nuevo_detalle_stock",
          "Detalle de esta presentación",
          placeholder = "Ejemplo: caja empezada, talle M..."
        ),

        checkboxInput(
          "nuevo_es_compra",
          "Marcar este material para comprar",
          value = FALSE
        ),

        div(
          class = "fila-formulario-dos",
          textInput(
            "nueva_cantidad_compra",
            "Cantidad a comprar",
            value = ""
          ),
          textInput(
            "nuevo_precio_compra",
            "Precio de referencia",
            value = ""
          )
        ),

        textAreaInput(
          "nuevo_comentario",
          "Comentario",
          rows = 2
        ),

        textAreaInput(
          "nueva_observacion",
          "Observaciones",
          rows = 3
        ),

        footer = tagList(
          modalButton("Cancelar"),
          actionButton(
            "guardar_nuevo_material",
            "Guardar material",
            class = "btn-principal"
          )
        )
      )
    )
  })

  observeEvent(input$guardar_nuevo_material, {
    ejecutar_accion(
      function() {
        cantidad <- leer_numero(
          input$nueva_cantidad,
          "Cantidad actual",
          permitir_vacio = TRUE
        )

        contenido <- leer_numero(
          input$nuevo_contenido,
          "Contenido por unidad",
          permitir_vacio = TRUE
        )

        agregar_material(
          ruta_db = RUTAS$base_datos,
          nombre = input$nuevo_nombre,
          categoria_id = input$nueva_categoria,
          cantidad = cantidad,
          unidad = input$nueva_unidad,
          contenido_por_unidad = contenido,
          comentarios = input$nuevo_comentario,
          observaciones = input$nueva_observacion,
          detalle_stock = input$nuevo_detalle_stock,
          es_compra = input$nuevo_es_compra,
          cantidad_compra = input$nueva_cantidad_compra,
          precio_compra = input$nuevo_precio_compra,
          origen = "manual"
        )
      },
      mensaje_exito = "Material agregado correctamente.",
      cerrar_modal = TRUE
    )
  }, ignoreInit = TRUE)

  # ----------------------------------------------------------
  # Editar material
  # ----------------------------------------------------------

  observeEvent(input$material_edit_action, {
    req(input$material_edit_action$id)

    material_id <- as.integer(input$material_edit_action$id)
    material_seleccionado(material_id)

    datos_material <- obtener_material(
      RUTAS$base_datos,
      material_id
    )$material

    if (nrow(datos_material) == 0) {
      showNotification("No se encontró el material.", type = "error")
      return()
    }

    categorias <- obtener_choices_categorias()

    showModal(
      modalDialog(
        title = "Editar material",
        size = "l",
        easyClose = FALSE,

        textInput(
          "editar_nombre",
          "Nombre del material",
          value = datos_material$nombre[[1]]
        ),

        selectInput(
          "editar_categoria",
          "Categoría",
          choices = categorias,
          selected = as.character(datos_material$categoria_id[[1]])
        ),

        textAreaInput(
          "editar_comentario",
          "Comentario",
          value = datos_material$comentarios[[1]],
          rows = 2
        ),

        textAreaInput(
          "editar_observacion",
          "Observaciones",
          value = datos_material$observaciones[[1]],
          rows = 3
        ),

        footer = tagList(
          modalButton("Cancelar"),
          actionButton(
            "guardar_edicion_material",
            "Guardar cambios",
            class = "btn-principal"
          )
        )
      )
    )
  })

  observeEvent(input$guardar_edicion_material, {
    req(material_seleccionado())

    ejecutar_accion(
      function() {
        actualizar_material(
          ruta_db = RUTAS$base_datos,
          material_id = material_seleccionado(),
          nombre = input$editar_nombre,
          categoria_id = input$editar_categoria,
          comentarios = input$editar_comentario,
          observaciones = input$editar_observacion
        )
      },
      mensaje_exito = "Material actualizado.",
      cerrar_modal = TRUE
    )
  }, ignoreInit = TRUE)

  # ----------------------------------------------------------
  # Eliminar material
  # ----------------------------------------------------------

  observeEvent(input$material_delete_action, {
    req(input$material_delete_action$id)

    material_id <- as.integer(input$material_delete_action$id)
    material_a_eliminar(material_id)

    datos <- obtener_material(RUTAS$base_datos, material_id)$material
    nombre <- if (nrow(datos) > 0) datos$nombre[[1]] else "este material"

    showModal(
      modalDialog(
        title = "Eliminar material",
        tags$p(
          paste0(
            "Se eliminará “",
            nombre,
            "” junto con todas sus presentaciones de stock."
          )
        ),
        tags$p("Esta acción no se puede deshacer."),
        easyClose = FALSE,

        footer = tagList(
          modalButton("Cancelar"),
          actionButton(
            "confirmar_eliminar_material",
            "Eliminar",
            class = "btn-peligro"
          )
        )
      )
    )
  })

  observeEvent(input$confirmar_eliminar_material, {
    req(material_a_eliminar())

    ejecutar_accion(
      function() {
        eliminar_material(
          RUTAS$base_datos,
          material_a_eliminar()
        )
      },
      mensaje_exito = "Material eliminado.",
      cerrar_modal = TRUE
    )
  }, ignoreInit = TRUE)

  # ----------------------------------------------------------
  # Ajustar stock con flechas
  # ----------------------------------------------------------

  observeEvent(input$stock_action, {
    req(input$stock_action$id)
    req(input$stock_action$delta)

    ejecutar_accion(
      function() {
        ajustar_stock(
          ruta_db = RUTAS$base_datos,
          stock_id = as.integer(input$stock_action$id),
          cambio = as.numeric(input$stock_action$delta)
        )
      }
    )
  })

  # ----------------------------------------------------------
  # Añadir otra presentación de stock
  # ----------------------------------------------------------

  observeEvent(input$material_add_stock_action, {
    req(input$material_add_stock_action$id)

    material_id <- as.integer(input$material_add_stock_action$id)
    material_seleccionado(material_id)

    datos <- obtener_material(RUTAS$base_datos, material_id)$material
    nombre <- if (nrow(datos) > 0) datos$nombre[[1]] else "Material"

    showModal(
      modalDialog(
        title = paste0("Añadir stock: ", nombre),
        size = "m",
        easyClose = FALSE,

        div(
          class = "fila-formulario",

          textInput(
            "stock_nueva_cantidad",
            "Cantidad",
            value = "0",
            placeholder = "Vacío para S/C"
          ),

          textInput(
            "stock_nueva_unidad",
            "Unidad",
            value = "unidad"
          ),

          textInput(
            "stock_nuevo_contenido",
            "Contenido por unidad",
            value = ""
          )
        ),

        textInput(
          "stock_nuevo_detalle",
          "Detalle",
          placeholder = "Ejemplo: unidades sueltas, caja cerrada..."
        ),

        footer = tagList(
          modalButton("Cancelar"),
          actionButton(
            "guardar_nuevo_stock",
            "Guardar stock",
            class = "btn-principal"
          )
        )
      )
    )
  })

  observeEvent(input$guardar_nuevo_stock, {
    req(material_seleccionado())

    ejecutar_accion(
      function() {
        cantidad <- leer_numero(
          input$stock_nueva_cantidad,
          "Cantidad",
          permitir_vacio = TRUE
        )

        contenido <- leer_numero(
          input$stock_nuevo_contenido,
          "Contenido por unidad",
          permitir_vacio = TRUE
        )

        agregar_stock(
          ruta_db = RUTAS$base_datos,
          material_id = material_seleccionado(),
          cantidad = cantidad,
          unidad = input$stock_nueva_unidad,
          contenido_por_unidad = contenido,
          detalle = input$stock_nuevo_detalle
        )
      },
      mensaje_exito = "Stock añadido.",
      cerrar_modal = TRUE
    )
  }, ignoreInit = TRUE)

  # ----------------------------------------------------------
  # Editar presentación de stock
  # ----------------------------------------------------------

  observeEvent(input$stock_edit_action, {
    req(input$stock_edit_action$id)

    stock_id <- as.integer(input$stock_edit_action$id)
    stock_seleccionado(stock_id)

    inventario <- obtener_inventario(RUTAS$base_datos)

    fila <- inventario[
      !is.na(inventario$stock_id) &
        inventario$stock_id == stock_id,
      ,
      drop = FALSE
    ]

    if (nrow(fila) == 0) {
      showNotification(
        "No se encontró esa presentación de stock.",
        type = "error"
      )
      return()
    }

    showModal(
      modalDialog(
        title = paste0("Editar stock: ", fila$material[[1]]),
        size = "m",
        easyClose = FALSE,

        div(
          class = "fila-formulario",

          textInput(
            "stock_editar_cantidad",
            "Cantidad",
            value = numero_para_input(fila$cantidad[[1]])
          ),

          textInput(
            "stock_editar_unidad",
            "Unidad",
            value = fila$unidad[[1]]
          ),

          textInput(
            "stock_editar_contenido",
            "Contenido por unidad",
            value = numero_para_input(
              fila$contenido_por_unidad[[1]]
            )
          )
        ),

        textInput(
          "stock_editar_detalle",
          "Detalle",
          value = fila$detalle_stock[[1]]
        ),

        footer = tagList(
          modalButton("Cancelar"),
          actionButton(
            "guardar_edicion_stock",
            "Guardar cambios",
            class = "btn-principal"
          )
        )
      )
    )
  })

  observeEvent(input$guardar_edicion_stock, {
    req(stock_seleccionado())

    ejecutar_accion(
      function() {
        cantidad <- leer_numero(
          input$stock_editar_cantidad,
          "Cantidad",
          permitir_vacio = TRUE
        )

        contenido <- leer_numero(
          input$stock_editar_contenido,
          "Contenido por unidad",
          permitir_vacio = TRUE
        )

        actualizar_stock(
          ruta_db = RUTAS$base_datos,
          stock_id = stock_seleccionado(),
          cantidad = cantidad,
          unidad = input$stock_editar_unidad,
          contenido_por_unidad = contenido,
          detalle = input$stock_editar_detalle
        )
      },
      mensaje_exito = "Stock actualizado.",
      cerrar_modal = TRUE
    )
  }, ignoreInit = TRUE)

  # ----------------------------------------------------------
  # Eliminar presentación de stock
  # ----------------------------------------------------------

  observeEvent(input$stock_delete_action, {
    req(input$stock_delete_action$id)

    stock_a_eliminar(as.integer(input$stock_delete_action$id))

    showModal(
      modalDialog(
        title = "Eliminar presentación de stock",
        tags$p(
          "Se eliminará esta línea de cantidad y unidad del material."
        ),
        easyClose = FALSE,

        footer = tagList(
          modalButton("Cancelar"),
          actionButton(
            "confirmar_eliminar_stock",
            "Eliminar",
            class = "btn-peligro"
          )
        )
      )
    )
  })

  observeEvent(input$confirmar_eliminar_stock, {
    req(stock_a_eliminar())

    ejecutar_accion(
      function() {
        eliminar_stock(
          RUTAS$base_datos,
          stock_a_eliminar()
        )
      },
      mensaje_exito = "Presentación eliminada.",
      cerrar_modal = TRUE
    )
  }, ignoreInit = TRUE)

  # ----------------------------------------------------------
  # Marcar o editar compra pendiente
  # ----------------------------------------------------------

  observeEvent(input$material_purchase_action, {
    req(input$material_purchase_action$id)

    material_id <- as.integer(input$material_purchase_action$id)
    material_compra_seleccionado(material_id)

    datos <- obtener_material(RUTAS$base_datos, material_id)$material

    if (nrow(datos) == 0) {
      showNotification("No se encontró el material.", type = "error")
      return()
    }

    showModal(
      modalDialog(
        title = paste0("Compra pendiente: ", datos$nombre[[1]]),
        size = "m",
        easyClose = FALSE,

        textInput(
          "compra_cantidad",
          "Cantidad a comprar",
          value = datos$cantidad_compra[[1]],
          placeholder = "Ejemplo: 2 cajas"
        ),

        textInput(
          "compra_precio",
          "Precio de referencia",
          value = datos$precio_compra[[1]],
          placeholder = "Opcional"
        ),

        footer = tagList(
          modalButton("Cancelar"),
          actionButton(
            "guardar_compra_pendiente",
            "Guardar compra",
            class = "btn-principal"
          )
        )
      )
    )
  })

  observeEvent(input$guardar_compra_pendiente, {
    req(material_compra_seleccionado())

    ejecutar_accion(
      function() {
        marcar_para_comprar(
          ruta_db = RUTAS$base_datos,
          material_id = material_compra_seleccionado(),
          cantidad_compra = input$compra_cantidad,
          precio_compra = input$compra_precio
        )
      },
      mensaje_exito = "Compra pendiente guardada.",
      cerrar_modal = TRUE
    )
  }, ignoreInit = TRUE)

  observeEvent(input$material_purchase_clear_action, {
    req(input$material_purchase_clear_action$id)

    ejecutar_accion(
      function() {
        quitar_marca_compra(
          ruta_db = RUTAS$base_datos,
          material_id = as.integer(
            input$material_purchase_clear_action$id
          )
        )
      },
      mensaje_exito = paste0(
        "Se quitó la marca manual. ",
        "Si el stock está en 0 o S/C, seguirá figurando para comprar."
      )
    )
  })

  # ----------------------------------------------------------
  # Registrar compra realizada
  # ----------------------------------------------------------

  observeEvent(input$material_purchase_received_action, {
    req(input$material_purchase_received_action$id)

    material_id <- as.integer(
      input$material_purchase_received_action$id
    )

    material_compra_seleccionado(material_id)

    datos_material <- obtener_material(
      RUTAS$base_datos,
      material_id
    )

    if (nrow(datos_material$material) == 0) {
      showNotification("No se encontró el material.", type = "error")
      return()
    }

    showModal(
      modalDialog(
        title = paste0(
          "Registrar compra recibida: ",
          datos_material$material$nombre[[1]]
        ),
        size = "l",
        easyClose = FALSE,

        selectInput(
          "compra_stock_destino",
          "¿Dónde sumar la compra?",
          choices = crear_choices_stock(datos_material$stock),
          selected = ""
        ),

        div(
          class = "fila-formulario",

          textInput(
            "compra_cantidad_recibida",
            "Cantidad recibida",
            value = "1"
          ),

          textInput(
            "compra_unidad_recibida",
            "Unidad",
            value = "unidad"
          ),

          textInput(
            "compra_contenido_recibido",
            "Contenido por unidad",
            value = ""
          )
        ),

        textInput(
          "compra_detalle_recibido",
          "Detalle",
          value = "Compra recibida"
        ),

        tags$p(
          class = "material-texto-secundario",
          paste0(
            "Si elegís una presentación existente, solo se sumará la ",
            "cantidad recibida. La unidad y el contenido se usarán ",
            "cuando crees una presentación nueva."
          )
        ),

        footer = tagList(
          modalButton("Cancelar"),
          actionButton(
            "confirmar_compra_realizada",
            "Registrar recepción",
            class = "btn-principal"
          )
        )
      )
    )
  })

  observeEvent(input$confirmar_compra_realizada, {
    req(material_compra_seleccionado())

    ejecutar_accion(
      function() {
        cantidad <- leer_numero(
          input$compra_cantidad_recibida,
          "Cantidad recibida",
          permitir_vacio = FALSE
        )

        contenido <- leer_numero(
          input$compra_contenido_recibido,
          "Contenido por unidad",
          permitir_vacio = TRUE
        )

        stock_destino <- limpiar_texto(input$compra_stock_destino)
        stock_id <- if (nzchar(stock_destino)) {
          as.integer(stock_destino)
        } else {
          NULL
        }

        registrar_compra_realizada(
          ruta_db = RUTAS$base_datos,
          material_id = material_compra_seleccionado(),
          cantidad_recibida = cantidad,
          unidad = input$compra_unidad_recibida,
          contenido_por_unidad = contenido,
          detalle = input$compra_detalle_recibido,
          stock_id = stock_id
        )
      },
      mensaje_exito = "La compra fue incorporada al stock.",
      cerrar_modal = TRUE
    )
  }, ignoreInit = TRUE)

  # ----------------------------------------------------------
  # Gestión de categorías
  # ----------------------------------------------------------

  observeEvent(input$abrir_categorias, {
    showModal(
      modalDialog(
        title = "Gestionar categorías",
        size = "m",
        easyClose = TRUE,

        textInput(
          "nueva_categoria_nombre",
          "Nueva categoría",
          placeholder = "Escribí el nombre"
        ),

        actionButton(
          "guardar_nueva_categoria",
          "Añadir categoría",
          class = "btn-principal"
        ),

        div(class = "separador-modal"),

        tags$h4("Categorías disponibles"),
        uiOutput("categorias_gestion_ui"),

        footer = modalButton("Cerrar")
      )
    )
  })

  output$categorias_gestion_ui <- renderUI({
    req(autenticado())

    revision_remota()
    version_local()

    categorias <- obtener_categorias(RUTAS$base_datos)

    if (nrow(categorias) == 0) {
      return(
        div(
          class = "material-texto-secundario",
          "No hay categorías."
        )
      )
    }

    tagList(
      lapply(
        seq_len(nrow(categorias)),
        function(i) {
          categoria <- categorias[i, ]

          div(
            class = "categoria-gestion-fila",

            div(
              class = "categoria-gestion-nombre",
              categoria$nombre[[1]]
            ),

            boton_evento(
              "Eliminar",
              "category_delete_action",
              sprintf(
                "{id: %d, nonce: Date.now()}",
                as.integer(categoria$id[[1]])
              ),
              clase = "categoria-gestion-eliminar",
              titulo = "Solo puede eliminarse si no contiene materiales"
            )
          )
        }
      )
    )
  })

  observeEvent(input$guardar_nueva_categoria, {
    ejecutar_accion(
      function() {
        agregar_categoria(
          RUTAS$base_datos,
          input$nueva_categoria_nombre
        )
      },
      mensaje_exito = "Categoría añadida."
    )

    updateTextInput(
      session,
      "nueva_categoria_nombre",
      value = ""
    )
  }, ignoreInit = TRUE)

  observeEvent(input$category_delete_action, {
    req(input$category_delete_action$id)

    ejecutar_accion(
      function() {
        eliminar_categoria(
          RUTAS$base_datos,
          as.integer(input$category_delete_action$id)
        )
      },
      mensaje_exito = "Categoría eliminada."
    )
  })

  # ----------------------------------------------------------
  # Importar inventario o lista de compras desde Word
  # ----------------------------------------------------------

  observeEvent(input$abrir_importar_word, {
    showModal(
      modalDialog(
        title = "Importar archivo Word",
        size = "m",
        easyClose = FALSE,

        tags$p(
          "El programa reconoce automáticamente:"
        ),

        tags$ul(
          tags$li(
            tags$strong("Inventarios: "),
            "Cantidad, Elemento y Observaciones."
          ),
          tags$li(
            tags$strong("Listas de compras: "),
            "Cantidad, Elemento y Precio."
          )
        ),

        fileInput(
          "archivo_word",
          "Archivo Word",
          accept = c(
            ".docx",
            "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
          ),
          buttonLabel = "Examinar...",
          placeholder = "Ningún archivo seleccionado"
        ),

        tags$div(
          class = "material-texto-secundario",
          tags$p(
            "Antes de importar se genera automáticamente un backup."
          ),
          tags$p(
            paste0(
              "Los materiales de una lista de compras se muestran en rojo ",
              "con la etiqueta COMPRAR."
            )
          )
        ),

        footer = tagList(
          modalButton("Cancelar"),
          actionButton(
            "confirmar_importar_word",
            "Importar archivo",
            class = "btn-principal"
          )
        )
      )
    )
  })

  observeEvent(input$confirmar_importar_word, {
    req(input$archivo_word)

    archivo_subido <- input$archivo_word
    extension_original <- tolower(
      tools::file_ext(archivo_subido$name)
    )

    if (!identical(extension_original, "docx")) {
      showNotification(
        "El archivo seleccionado debe tener extensión .docx.",
        type = "error",
        duration = 8
      )
      return()
    }

    archivo_temporal <- tempfile(
      pattern = "inventario_verto_",
      fileext = ".docx"
    )

    copiado <- file.copy(
      from = archivo_subido$datapath,
      to = archivo_temporal,
      overwrite = TRUE
    )

    if (!isTRUE(copiado)) {
      showNotification(
        "No se pudo preparar el archivo Word.",
        type = "error",
        duration = 8
      )
      return()
    }

    on.exit(
      unlink(archivo_temporal, force = TRUE),
      add = TRUE
    )

    resultado <- tryCatch(
      importar_inventario_docx(
        ruta_db = RUTAS$base_datos,
        archivo_docx = archivo_temporal,
        crear_backup_previo = TRUE
      ),
      error = function(e) {
        showNotification(
          conditionMessage(e),
          type = "error",
          duration = 13
        )
        NULL
      }
    )

    if (is.null(resultado)) return()

    removeModal()
    refrescar()

    tipo_texto <- if (
      identical(resultado$tipo_documento, "compra")
    ) {
      "Lista de compras"
    } else {
      "Inventario"
    }

    resumen_importacion <- tagList(
      tags$p(
        tags$strong("Archivo: "),
        archivo_subido$name
      ),
      tags$p(
        tags$strong("Tipo detectado: "),
        tipo_texto
      ),
      tags$ul(
        tags$li(
          paste0(
            "Filas detectadas: ",
            resultado$total_detectado
          )
        ),
        tags$li(
          paste0(
            "Materiales nuevos: ",
            resultado$importados
          )
        ),
        tags$li(
          paste0(
            "Compras detectadas: ",
            resultado$compras_detectadas
          )
        ),
        tags$li(
          paste0(
            "Materiales existentes marcados para comprar: ",
            resultado$actualizados_compra
          )
        ),
        tags$li(
          paste0(
            "Duplicados omitidos: ",
            resultado$omitidos
          )
        ),
        tags$li(
          paste0(
            "Categorías nuevas: ",
            resultado$categorias_nuevas
          )
        )
      )
    )

    if (
      length(resultado$omitidos_detalle) > 0 &&
      resultado$omitidos > 0
    ) {
      limite <- min(length(resultado$omitidos_detalle), 10L)

      resumen_importacion <- tagList(
        resumen_importacion,
        div(class = "separador-modal"),
        tags$p(tags$strong("Duplicados omitidos:")),
        tags$ul(
          lapply(
            resultado$omitidos_detalle[seq_len(limite)],
            tags$li
          )
        ),
        if (length(resultado$omitidos_detalle) > limite) {
          tags$p(
            class = "material-texto-secundario",
            paste0(
              "Y ",
              length(resultado$omitidos_detalle) - limite,
              " duplicados más."
            )
          )
        }
      )
    }

    if (
      !is.null(resultado$backup) &&
      nzchar(resultado$backup)
    ) {
      resumen_importacion <- tagList(
        resumen_importacion,
        tags$p(
          class = "material-texto-secundario",
          paste0(
            "Backup previo generado: ",
            basename(resultado$backup)
          )
        )
      )
    }

    showModal(
      modalDialog(
        title = "Importación terminada",
        resumen_importacion,
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      )
    )
  }, ignoreInit = TRUE)

  # ----------------------------------------------------------
  # Backup descargable
  # ----------------------------------------------------------

  output$descargar_backup <- downloadHandler(
    filename = function() {
      extension <- if (
        requireNamespace("writexl", quietly = TRUE)
      ) {
        ".xlsx"
      } else {
        ".rds"
      }

      paste0(
        "Inventario_Verto_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        extension
      )
    },

    content = function(file) {
      origen <- crear_backup(RUTAS$base_datos)

      copiado <- file.copy(
        origen,
        file,
        overwrite = TRUE
      )

      unlink(origen, force = TRUE)

      if (!isTRUE(copiado)) {
        stop("No se pudo preparar el archivo de backup.")
      }
    }
  )

  # Aviso local cuando todavía no se configuró una clave.
  observe({
    req(autenticado())

    if (!REQUIERE_CLAVE) {
      showNotification(
        paste0(
          "APP_PASSWORD todavía no está configurada. ",
          "La aplicación está abierta sin clave de acceso."
        ),
        type = "warning",
        duration = 8
      )
    }
  }) |> bindEvent(autenticado(), once = TRUE)
}

shinyApp(
  ui = ui,
  server = server,
  options = list(
    launch.browser = TRUE
  )
)
