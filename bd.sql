CREATE TABLE EMPLEADO (
    ID NUMBER,
    NOMBRE VARCHAR(20),
    CEDULA VARCHAR(20),
    TELEFONO VARCHAR(20),
    PROVINCIA VARCHAR(20),
    SALARIO NUMBER,
    PRIMARY KEY(ID)
);

INSERT INTO EMPLEADO VALUES(
    1,
    'Marta Porras',
    '2-0066-0976',
    '8896-8976',
    'San Jose',
    850000
);

INSERT INTO EMPLEADO VALUES(
    2,
    'Pedro Perez',
    '1-8765-1234',
    '8765-0987',
    'San Jose',
    456000
);

INSERT INTO EMPLEADO VALUES(
    3,
    'Gloria Flores',
    '1-5555-8888',
    '8666-0555',
    'Heredia',
    987000
);

SET AUTOCOMMIT OFF;

COMMIT;

SELECT
    *
FROM
    EMPLEADO;

SET TRANSACTION NAME 'T1';

UPDATE EMPLEADO
SET
    CEDULACEDULA = '1-5555-8888'
WHERE
    IDID = 3;

SAVEPOINT CAMBIOS_CEDULA;

UPDATE EMPLEADO
SET
    CEDULACEDULA = '1-2222-0333'
WHERE
    IDID = 2;

SELECT
    SUM(SALARIO)
FROM
    EMPLEADO;

ROLLBACK TO SAVEPOINT CAMBIOS_CEDULA;

SAVEPOINT CAMBIO_SALARIO;

UPDATE EMPLEADO
SET
    SALARIO = 999000
WHERE
    ID = 3;

CREATE TABLE PUESTO(
    PUESTO_ID NUMBER,
    NOMBRE VARCHAR2(30)
);

ROLLBACK TO SAVEPOINT CAMBIO_SALARIO;

UPDATE EMPLEADO
SET
    SALARIO = 900000
WHERE
    ID = 1;

COMMIT;

-----------------------------------------------------------
CREATE ROLE contador WITH LOGIN PASSWORD '123456'
VALID UNTIL '2025-12-01';
CREATE USER newUser WITH PASSWORD '123456789';
SELECT * FROM pg_user; 
ALTER USER newUser CREATEDB;
ALTER ROLE contador RENAME TO nuevoNombre;

drop role nuevoNombre;
drop user newUser;

-- ----
CREATE TABLE especie(id serial PRIMARY KEY, nombre_cientifico varchar);
CREATE user newUser WITH PASSWORD 'hola';

insert into especie values(1, 'Sapiens');
insert into especie values(2, 'Sapiens');
grant connect on database base to newUser;
Grant select on especie to newUser;

revoke connect on database base from newUser;

CREATE OR REPLACE VIEW vista_empleados AS
SELECT id, nombre, cedula, telefono, provincia
FROM empleados
WHERE id<>2;


select * from especie;