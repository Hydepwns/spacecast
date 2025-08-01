--
-- PostgreSQL database dump
--

-- Dumped from database version 15.13
-- Dumped by pg_dump version 15.13

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: event_notifications; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_notifications (
    id bigint NOT NULL,
    title character varying(255) NOT NULL,
    message character varying(255) NOT NULL,
    type character varying(255) NOT NULL,
    status character varying(255) NOT NULL,
    sent_at timestamp(0) without time zone,
    metadata jsonb,
    inserted_at timestamp(0) without time zone NOT NULL,
    updated_at timestamp(0) without time zone NOT NULL
);


--
-- Name: event_notifications_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_notifications_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_notifications_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_notifications_id_seq OWNED BY public.event_notifications.id;


--
-- Name: event_reminders; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_reminders (
    id uuid NOT NULL,
    reminder_time timestamp(0) without time zone NOT NULL,
    status character varying(255) NOT NULL,
    recipient character varying(255) NOT NULL,
    sent_at timestamp(0) without time zone,
    error_message text,
    event_id uuid NOT NULL,
    inserted_at timestamp(0) without time zone NOT NULL,
    updated_at timestamp(0) without time zone NOT NULL
);


--
-- Name: event_replay_sessions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_replay_sessions (
    id uuid NOT NULL,
    name character varying(255) NOT NULL,
    _resource_type character varying(255) NOT NULL,
    _resource_id character varying(255) NOT NULL,
    start_event_id uuid,
    end_event_id uuid,
    status character varying(255) DEFAULT 'pending'::character varying,
    _metadata jsonb DEFAULT '{}'::jsonb,
    results jsonb DEFAULT '{}'::jsonb,
    inserted_at timestamp(0) without time zone NOT NULL,
    updated_at timestamp(0) without time zone NOT NULL
);


--
-- Name: event_settings; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_settings (
    id uuid NOT NULL,
    timezone character varying(255) DEFAULT 'UTC'::character varying NOT NULL,
    default_status character varying(255) DEFAULT 'draft'::character varying NOT NULL,
    default_duration integer DEFAULT 60 NOT NULL,
    max_events_per_day integer DEFAULT 10 NOT NULL,
    enable_reminders boolean DEFAULT true,
    reminder_time integer DEFAULT 24,
    event_id uuid NOT NULL,
    inserted_at timestamp(0) without time zone NOT NULL,
    updated_at timestamp(0) without time zone NOT NULL
);


--
-- Name: events; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.events (
    id uuid NOT NULL,
    type character varying(255) NOT NULL,
    resource_id character varying(255) NOT NULL,
    resource_type character varying(255) NOT NULL,
    data jsonb DEFAULT '{}'::jsonb,
    metadata jsonb DEFAULT '{}'::jsonb,
    correlation_id uuid,
    causation_id uuid,
    "timestamp" timestamp without time zone NOT NULL,
    inserted_at timestamp(0) without time zone NOT NULL,
    updated_at timestamp(0) without time zone NOT NULL
);


--
-- Name: resource_snapshots; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.resource_snapshots (
    id uuid NOT NULL,
    resource_type character varying(255) NOT NULL,
    resource_id character varying(255) NOT NULL,
    state jsonb NOT NULL,
    metadata jsonb DEFAULT '{}'::jsonb,
    inserted_at timestamp(0) without time zone NOT NULL,
    updated_at timestamp(0) without time zone NOT NULL
);


--
-- Name: resources; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.resources (
    id uuid NOT NULL,
    name character varying(255) NOT NULL,
    type character varying(255) NOT NULL,
    status character varying(255) NOT NULL,
    description character varying(255),
    content jsonb DEFAULT '{}'::jsonb,
    metadata jsonb DEFAULT '{}'::jsonb,
    settings jsonb DEFAULT '{}'::jsonb,
    version integer DEFAULT 1,
    parent_id uuid,
    child_ids uuid[],
    tags character varying(255)[],
    categories character varying(255)[],
    created_by uuid,
    updated_by uuid,
    inserted_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL
);


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.schema_migrations (
    version bigint NOT NULL,
    inserted_at timestamp(0) without time zone
);


--
-- Name: themes; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.themes (
    id bigint NOT NULL,
    name character varying(255) NOT NULL,
    mode character varying(255) NOT NULL,
    primary_color character varying(255) NOT NULL,
    secondary_color character varying(255) NOT NULL,
    background_color character varying(255) NOT NULL,
    text_color character varying(255) NOT NULL,
    is_default boolean DEFAULT false NOT NULL,
    inserted_at timestamp(0) without time zone NOT NULL,
    updated_at timestamp(0) without time zone NOT NULL,
    settings jsonb DEFAULT '{}'::jsonb
);


--
-- Name: themes_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.themes_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: themes_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.themes_id_seq OWNED BY public.themes.id;


--
-- Name: users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.users (
    id bigint NOT NULL,
    name character varying(255) NOT NULL,
    email character varying(255) NOT NULL,
    password_hash character varying(255) NOT NULL,
    role character varying(255) DEFAULT 'user'::character varying,
    active boolean DEFAULT true,
    email_confirmed_at timestamp(0) without time zone,
    password_reset_token character varying(255),
    password_reset_sent_at timestamp(0) without time zone,
    confirmed_at timestamp(0) without time zone,
    inserted_at timestamp(0) without time zone NOT NULL,
    updated_at timestamp(0) without time zone NOT NULL
);


--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.users_id_seq OWNED BY public.users.id;


--
-- Name: users_tokens; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.users_tokens (
    id bigint NOT NULL,
    token bytea NOT NULL,
    context character varying(255) NOT NULL,
    sent_to character varying(255),
    user_id bigint NOT NULL,
    inserted_at timestamp(0) without time zone NOT NULL
);


--
-- Name: users_tokens_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.users_tokens_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: users_tokens_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.users_tokens_id_seq OWNED BY public.users_tokens.id;


--
-- Name: versioned_states; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.versioned_states (
    id uuid NOT NULL,
    resource_type character varying(255) NOT NULL,
    resource_id character varying(255) NOT NULL,
    state jsonb NOT NULL,
    label character varying(255) NOT NULL,
    replay_id uuid,
    point_in_time timestamp without time zone,
    metadata jsonb DEFAULT '{}'::jsonb,
    created_at timestamp without time zone NOT NULL
);


--
-- Name: event_notifications id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_notifications ALTER COLUMN id SET DEFAULT nextval('public.event_notifications_id_seq'::regclass);


--
-- Name: themes id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.themes ALTER COLUMN id SET DEFAULT nextval('public.themes_id_seq'::regclass);


--
-- Name: users id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);


--
-- Name: users_tokens id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users_tokens ALTER COLUMN id SET DEFAULT nextval('public.users_tokens_id_seq'::regclass);


--
-- Name: event_notifications event_notifications_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_notifications
    ADD CONSTRAINT event_notifications_pkey PRIMARY KEY (id);


--
-- Name: event_reminders event_reminders_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_reminders
    ADD CONSTRAINT event_reminders_pkey PRIMARY KEY (id);


--
-- Name: event_replay_sessions event_replay_sessions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_replay_sessions
    ADD CONSTRAINT event_replay_sessions_pkey PRIMARY KEY (id);


--
-- Name: event_settings event_settings_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_settings
    ADD CONSTRAINT event_settings_pkey PRIMARY KEY (id);


--
-- Name: events events_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.events
    ADD CONSTRAINT events_pkey PRIMARY KEY (id);


--
-- Name: resource_snapshots resource_snapshots_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.resource_snapshots
    ADD CONSTRAINT resource_snapshots_pkey PRIMARY KEY (id);


--
-- Name: resources resources_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.resources
    ADD CONSTRAINT resources_pkey PRIMARY KEY (id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: themes themes_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.themes
    ADD CONSTRAINT themes_pkey PRIMARY KEY (id);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: users_tokens users_tokens_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users_tokens
    ADD CONSTRAINT users_tokens_pkey PRIMARY KEY (id);


--
-- Name: versioned_states versioned_states_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.versioned_states
    ADD CONSTRAINT versioned_states_pkey PRIMARY KEY (id);


--
-- Name: event_notifications_status_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_notifications_status_index ON public.event_notifications USING btree (status);


--
-- Name: event_notifications_type_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_notifications_type_index ON public.event_notifications USING btree (type);


--
-- Name: event_reminders_event_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_reminders_event_id_index ON public.event_reminders USING btree (event_id);


--
-- Name: event_reminders_reminder_time_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_reminders_reminder_time_index ON public.event_reminders USING btree (reminder_time);


--
-- Name: event_reminders_reminder_time_status_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_reminders_reminder_time_status_index ON public.event_reminders USING btree (reminder_time, status);


--
-- Name: event_reminders_status_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_reminders_status_index ON public.event_reminders USING btree (status);


--
-- Name: event_replay_sessions__resource_type__resource_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_replay_sessions__resource_type__resource_id_index ON public.event_replay_sessions USING btree (_resource_type, _resource_id);


--
-- Name: event_replay_sessions_end_event_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_replay_sessions_end_event_id_index ON public.event_replay_sessions USING btree (end_event_id);


--
-- Name: event_replay_sessions_start_event_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_replay_sessions_start_event_id_index ON public.event_replay_sessions USING btree (start_event_id);


--
-- Name: event_replay_sessions_status_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_replay_sessions_status_index ON public.event_replay_sessions USING btree (status);


--
-- Name: event_settings_event_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_settings_event_id_index ON public.event_settings USING btree (event_id);


--
-- Name: events_causation_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX events_causation_id_index ON public.events USING btree (causation_id);


--
-- Name: events_correlation_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX events_correlation_id_index ON public.events USING btree (correlation_id);


--
-- Name: events_resource_id_resource_type_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX events_resource_id_resource_type_index ON public.events USING btree (resource_id, resource_type);


--
-- Name: events_timestamp_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX events_timestamp_index ON public.events USING btree ("timestamp");


--
-- Name: events_type_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX events_type_index ON public.events USING btree (type);


--
-- Name: resource_snapshots_inserted_at_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX resource_snapshots_inserted_at_index ON public.resource_snapshots USING btree (inserted_at);


--
-- Name: resource_snapshots_resource_type_resource_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX resource_snapshots_resource_type_resource_id_index ON public.resource_snapshots USING btree (resource_type, resource_id);


--
-- Name: resources_categories_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX resources_categories_index ON public.resources USING btree (categories);


--
-- Name: resources_created_by_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX resources_created_by_index ON public.resources USING btree (created_by);


--
-- Name: resources_parent_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX resources_parent_id_index ON public.resources USING btree (parent_id);


--
-- Name: resources_status_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX resources_status_index ON public.resources USING btree (status);


--
-- Name: resources_tags_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX resources_tags_index ON public.resources USING btree (tags);


--
-- Name: resources_type_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX resources_type_index ON public.resources USING btree (type);


--
-- Name: themes_is_default_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX themes_is_default_index ON public.themes USING btree (is_default);


--
-- Name: themes_name_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX themes_name_index ON public.themes USING btree (name);


--
-- Name: users_email_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX users_email_index ON public.users USING btree (email);


--
-- Name: users_tokens_context_token_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX users_tokens_context_token_index ON public.users_tokens USING btree (context, token);


--
-- Name: users_tokens_user_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX users_tokens_user_id_index ON public.users_tokens USING btree (user_id);


--
-- Name: versioned_states_replay_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX versioned_states_replay_id_index ON public.versioned_states USING btree (replay_id);


--
-- Name: versioned_states_resource_type_resource_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX versioned_states_resource_type_resource_id_index ON public.versioned_states USING btree (resource_type, resource_id);


--
-- Name: event_reminders event_reminders_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_reminders
    ADD CONSTRAINT event_reminders_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.events(id) ON DELETE CASCADE;


--
-- Name: users_tokens users_tokens_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users_tokens
    ADD CONSTRAINT users_tokens_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--

INSERT INTO public."schema_migrations" (version) VALUES (20250312063545);
INSERT INTO public."schema_migrations" (version) VALUES (20250313002054);
INSERT INTO public."schema_migrations" (version) VALUES (20250313180518);
INSERT INTO public."schema_migrations" (version) VALUES (20250516200807);
INSERT INTO public."schema_migrations" (version) VALUES (20250703211333);
INSERT INTO public."schema_migrations" (version) VALUES (20250707195202);
INSERT INTO public."schema_migrations" (version) VALUES (20250708111209);
INSERT INTO public."schema_migrations" (version) VALUES (20250708111237);
INSERT INTO public."schema_migrations" (version) VALUES (20250708111252);
INSERT INTO public."schema_migrations" (version) VALUES (20250708111559);
INSERT INTO public."schema_migrations" (version) VALUES (20250708111621);
INSERT INTO public."schema_migrations" (version) VALUES (20250710171829);
