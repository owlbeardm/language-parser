CREATE TABLE translations_tbl (
    id bigint NOT NULL,
    version bigint DEFAULT 1 NOT NULL,
    createdby text NOT NULL,
    createdwhen timestamp without time zone NOT NULL,
    modiby text NOT NULL,
    modiwhen timestamp without time zone NOT NULL,
    from_word_id bigint NOT NULL,
    to_lang_id bigint NOT NULL,
    to_word_id bigint,
    comment text,
    alt_translation text,
    CONSTRAINT translations_pk PRIMARY KEY (id),
    CONSTRAINT translations_name_unq CHECK ((to_word_id IS NULL AND alt_translation IS NOT NULL) OR (to_word_id IS NOT NULL AND alt_translation IS NULL))
);

    