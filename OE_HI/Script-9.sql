
-- Cambiar al esquema caces
SET search_path TO caces;

-- Tabla Universidades
CREATE TABLE universidades (
    idUniversidad SERIAL PRIMARY KEY,
    nombreUniversidad VARCHAR(255) NOT NULL,
    provincia VARCHAR(255)
);

-- Tabla Estudiantes
CREATE TABLE estudiantes (
    idEstudiante SERIAL PRIMARY KEY,
    nombre_estudiante VARCHAR(255) NOT NULL,
    carrera VARCHAR(255),
    fechaInicioMatriculacion DATE,
    idUniversidad INT REFERENCES universidades(idUniversidad)
);

-- Tabla Docentes
CREATE TABLE docentes (
    idDocente SERIAL PRIMARY KEY,
    nombre_docente VARCHAR(255) NOT NULL,
    edad_docente INT,
    grado VARCHAR(255),
    idUniversidad INT REFERENCES universidades(idUniversidad)
);


-- Inserciones en la tabla universidades
INSERT INTO universidades (nombreUniversidad, provincia) VALUES
    ('Universidad A', 'Provincia A'),
    ('Universidad B', 'Provincia B'),
    ('Universidad C', 'Provincia C'),
    ('Universidad D', 'Provincia D'),
    ('Universidad E', 'Provincia E'),
    ('Universidad F', 'Provincia F'),
    ('Universidad G', 'Provincia G'),
    ('Universidad H', 'Provincia H'),
    ('Universidad I', 'Provincia I'),
    ('Universidad J', 'Provincia J');

-- Inserciones en la tabla estudiantes
INSERT INTO estudiantes (nombre_estudiante, carrera, fechaInicioMatriculacion, idUniversidad) VALUES
    ('Estudiante 1', 'Carrera A', '2022-01-01', 1),
    ('Estudiante 2', 'Carrera B', '2022-02-01', 2),
    ('Estudiante 3', 'Carrera C', '2022-03-01', 3),
    ('Estudiante 4', 'Carrera A', '2022-04-01', 4),
    ('Estudiante 5', 'Carrera B', '2022-05-01', 5),
    ('Estudiante 6', 'Carrera C', '2022-06-01', 6),
    ('Estudiante 7', 'Carrera A', '2022-07-01', 7),
    ('Estudiante 8', 'Carrera B', '2022-08-01', 8),
    ('Estudiante 9', 'Carrera C', '2022-09-01', 9),
    ('Estudiante 10', 'Carrera A', '2022-10-01', 10);

-- Inserciones en la tabla docentes
INSERT INTO docentes (nombre_docente, edad_docente, grado, idUniversidad) VALUES
    ('Docente 1', 35, 'Doctorado', 1),
    ('Docente 2', 40, 'Maestría', 2),
    ('Docente 3', 45, 'Doctorado', 3),
    ('Docente 4', 38, 'Maestría', 4),
    ('Docente 5', 42, 'Doctorado', 5),
    ('Docente 6', 37, 'Maestría', 6),
    ('Docente 7', 50, 'Doctorado', 7),
    ('Docente 8', 48, 'Maestría', 8),
    ('Docente 9', 55, 'Doctorado', 9),
    ('Docente 10', 52, 'Maestría', 10);

-- SQL para la creación del reporte 1:

SELECT
    d.nombre_docente,
    d.edad_docente,
    d.grado,
    u.nombreUniversidad AS nombre_universidad,
    u.provincia
FROM
    docentes d
JOIN
    universidades u ON d.idUniversidad = u.idUniversidad
WHERE
    d.edad_docente > 40
    AND d.grado = 'Doctorado';

SELECT t1.h02
FROM cpv.hog_2022 t1;
   
SELECT t2.h02
FROM cpv.hog_2022 AS t2
JOIN cpv.diccionarios AS t1 ON t2.h02 = t1.codigo WHERE t1.codigo_variable = "h02";

-- Obtención de las etiquetas desde el diccionario:

SELECT t2.etiqueta AS h02
FROM cpv.hog_2022 AS t1
JOIN cpv.diccionarios AS t2 ON t1.h02 = t2.codigo WHERE t2.codigo_variable = 'h02';

-- Para dos variables:

SELECT t2.etiqueta AS h02, t3.etiqueta AS h03
FROM cpv.hog_2022 AS t1
JOIN cpv.diccionarios AS t2 ON t1.h02 = t2.codigo
JOIN cpv.diccionarios AS t3 ON t1.h03 = t3.codigo
WHERE t2.codigo_variable = 'h02' AND t3.codigo_variable = 'h03';

SELECT postgis_full_version(); 

CREATE EXTENSION postgis;


SELECT DISTINCT(categoria) FROM c_economico.db_luae;

SELECT * FROM "C_ECONOMICO"."STG_LUAE";

   
 CREATE TABLE registros (
    id SERIAL PRIMARY KEY,
    ciudad TEXT[],
    institucion TEXT[],
    grupo_poblacional TEXT[],
    edad INTEGER,
    sexo TEXT,
    estado_civil TEXT
);
  
SELECT MIN(fecha_infraccion) AS min_fecha_infraccion
FROM hi_total;

SELECT MAX(fecha_infraccion) AS min_fecha_infraccion
FROM hi_total;
 
SELECT edaddias FROM endi.f1_personas;

SELECT count(*) FROM data_lake.hi_historico_da hhd; 

SELECT pid, usename, application_name, client_addr, backend_start, state
FROM pg_stat_activity
WHERE datname = 'mdi_dwh';  -- Reemplaza 'mdi_dwh' con tu base de datos

SELECT pg_terminate_backend(pid)
FROM pg_stat_activity
WHERE datname = 'mdi_dwh'  -- Reemplaza con el nombre de tu base de datos
  AND pid <> pg_backend_pid();  -- Evita terminar la conexión actual

























