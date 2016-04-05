PROJECT = sumo_rest

CONFIG ?= test/test.config

DEPS = mixer cowboy iso8601 jiffy trails cowboy_swagger sumo_db
SHELL_DEPS = sync
TEST_DEPS = shotgun katana_test
BUILD_DEPS = inaka_mk hexer_mk
DEP_PLUGINS = inaka_mk hexer_mk

dep_mixer          = git https://github.com/inaka/mixer.git            0.1.5
dep_cowboy         = git https://github.com/ninenines/cowboy.git       2.0.0-pre.3
dep_iso8601        = git https://github.com/kivra/erlang_iso8601.git   b5d1d6e
dep_jiffy          = hex 0.14.7
dep_trails         = git https://github.com/inaka/cowboy-trails.git    bc92848
dep_cowboy_swagger = git https://github.com/inaka/cowboy-swagger.git   1.0.2
dep_sumo_db        = git https://github.com/inaka/sumo_db.git          01db604
dep_sync           = git https://github.com/rustyio/sync.git           11df81d
dep_shotgun        = hex 0.2.3
dep_katana_test    = git https://github.com/inaka/katana-test.git      0.0.5
dep_inaka_mk       = git https://github.com/inaka/inaka.mk.git         1.0.0
dep_hexer_mk       = git https://github.com/inaka/hexer.mk.git         1.1.0

include erlang.mk

ERLC_OPTS := +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

TEST_ERLC_OPTS += +debug_info
CT_OPTS += -cover test/cover.spec -vvv -erl_args -boot start_sasl -config ${CONFIG}

SHELL_OPTS += -name ${PROJECT}@`hostname` -config ${CONFIG} -boot start_sasl -s sync

EDOC_OPTS += todo, report_missing_types

erldocs:
	erldocs . -o docs
