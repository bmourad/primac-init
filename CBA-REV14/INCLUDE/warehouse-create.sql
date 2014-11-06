drop table WAREHOUSE_LOC
go
drop table WAREHOUSE
go
create table WAREHOUSE (
  ID          varchar(10),
  WHSE_DESC   varchar(30),
  ADDR1       varchar(30),
  ADDR2       varchar(30),
  CITY        varchar(20),
  STATE       varchar(10),
  ZIP         varchar(10),
  DIV         char(3),
  PROD_FLG    char(1),
  COUNTRY     varchar(10),
  PHONE       varchar(13),
  DEF_LOC     varchar(10),
  PRB_FLG     char(1),
  CONSTRAINT  PK_WHSE_ID    PRIMARY KEY (ID)
)
go
create table WAREHOUSE_LOC (
  WAREHOUSE_ID   varchar(10),
  MV             int,
  LOC            varchar(10),
  LOC_DESC       varchar(30),
  CONSTRAINT     PK_WHSE_LOC_ID PRIMARY KEY (WAREHOUSE_ID,LOC),
  CONSTRAINT     FK_WHSE_ID     FOREIGN KEY (WAREHOUSE_ID)
    REFERENCES   WAREHOUSE(ID)
)
go

