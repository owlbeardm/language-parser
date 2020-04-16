CREATE TABLE word_origin_tbl (
    id bigint DEFAULT nextval('pk_sequence')	 NOT NULL,
    word_id bigint NOT NULL,
    comment text,
    evolved_yn BOOLEAN NOT NULL DEFAULT FALSE,
    migrated_yn BOOLEAN NOT NULL DEFAULT FALSE,
    combined_yn BOOLEAN NOT NULL DEFAULT FALSE,
    derivated_yn BOOLEAN NOT NULL DEFAULT FALSE,
    CONSTRAINT word_origin_pk PRIMARY KEY (id),
    CONSTRAINT evolve_law_word_id_unq UNIQUE (word_id),
    CONSTRAINT word_origin_tbl_word_id_fkey FOREIGN KEY (word_id) REFERENCES word_tbl(id) ON DELETE CASCADE
);