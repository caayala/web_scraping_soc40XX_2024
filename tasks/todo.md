# TODO - Migración captura La Tercera (API)

- [x] Levantar contexto del script actual y workflow de GitHub Actions.
- [x] Reimplementar `class_11_captura.R` usando endpoint API y extraer 100 noticias.
- [x] Mantener columnas históricas (`titulo`, `url_noticia`, `tag`, `texto`, `url_imagen`, `periodista`, `tiempo`, `fecha_captura`) con `NA` cuando no exista dato.
- [x] Agregar campos nuevos disponibles desde la API sin romper histórico.
- [x] Ajustar deduplicación/unión con histórico para diferencias de columnas.
- [x] Actualizar dependencias del workflow para ejecución diaria.
- [x] Verificar script y documentar resultado de validación.

## Revisión
- `R -q -e 'parse(file = "slides/class_11/class_11_captura.R")'`: OK.
- `source("slides/class_11/class_11_captura.R")`: falla en este entorno por restricción de red/DNS (`Couldn't resolve host name`), no por sintaxis.
