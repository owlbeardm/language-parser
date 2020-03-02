CREATE TABLE word_tbl (
    id bigint NOT NULL,
    version bigint DEFAULT 1 NOT NULL,
    createdby text NOT NULL,
    createdwhen timestamp without time zone NOT NULL,
    modiby text NOT NULL,
    modiwhen timestamp without time zone NOT NULL,
    word text NOT NULL,
    lang_id bigint NOT NULL,
    CONSTRAINT word_pk PRIMARY KEY (id),
    CONSTRAINT word_word_lang_id_unq UNIQUE (word,lang_id),
    CONSTRAINT word_lang_id_fk FOREIGN KEY (lang_id) REFERENCES language_tbl(id)
);