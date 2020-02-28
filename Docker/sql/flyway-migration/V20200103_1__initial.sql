CREATE TABLE smth_tbl (
    id bigint NOT NULL,
    version bigint DEFAULT 1 NOT NULL,
    createdby text NOT NULL,
    createdwhen timestamp without time zone NOT NULL,
    modiby text NOT NULL,
    modiwhen timestamp without time zone NOT NULL,
    sometext text NOT NULL,
    CONSTRAINT smth_pk PRIMARY KEY (id),
    CONSTRAINT smth_sometext_unq UNIQUE (sometext)
);

CREATE INDEX wiki_smth_from_idx ON wiki_smth_tbl(from_i);
CREATE INDEX wiki_smth_to_idx ON wiki_smth_tbl(to_i);