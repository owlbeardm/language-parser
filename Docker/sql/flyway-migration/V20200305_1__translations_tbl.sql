CREATE TABLE translation_tbl (
    id bigint DEFAULT nextval('pk_sequence') NOT NULL,
    from_word_id bigint NOT NULL,
    to_lang_id bigint NOT NULL,
    to_word_id bigint,
    comment text,
    alt_translation text,
    CONSTRAINT translation_pk PRIMARY KEY (id),
    CONSTRAINT translation_name_unq CHECK ((to_word_id IS NULL AND alt_translation IS NOT NULL) OR (to_word_id IS NOT NULL AND alt_translation IS NULL)),
    CONSTRAINT translation_tbl_from_word_id_fkey FOREIGN KEY (from_word_id) REFERENCES word_tbl(id),
    CONSTRAINT translation_tbl_to_lang_id_fkey FOREIGN KEY (to_word_id) REFERENCES word_tbl(id),
    CONSTRAINT translation_tbl_to_word_id_fkey FOREIGN KEY (to_lang_id) REFERENCES language_tbl(id)
);

CREATE INDEX translation_alt_translation_idx ON translation_tbl(alt_translation);