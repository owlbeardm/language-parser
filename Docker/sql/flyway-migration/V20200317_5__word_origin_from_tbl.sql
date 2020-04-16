CREATE TABLE word_origin_from_tbl (
    id bigint DEFAULT nextval('pk_sequence')	 NOT NULL,
    word_from_id bigint NOT NULL,
    origin_id bigint NOT NULL,
    CONSTRAINT word_origin_from_pk PRIMARY KEY (id),
    CONSTRAINT word_origin_from_tbl_word_from_id_fkey FOREIGN KEY (word_from_id) REFERENCES word_tbl(id),
    CONSTRAINT word_origin_from_tbl_origin_id_fkey FOREIGN KEY (origin_id) REFERENCES word_origin_tbl(id) ON DELETE CASCADE
);