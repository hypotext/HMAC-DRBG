SHAPATH=~/vst/sha
LIBROOT=~/Desktop/Code/thesis/func_spec
COQC=coqc -I $(LIBROOT)/VST -R $(LIBROOT)/CompCert-2.5 -as compcert
VOFILES=DRBG_generate_function.vo DRBG_instantiate_function.vo DRBG_state_handle.vo HMAC256_DRBG_functional_prog.vo HMAC_DRBG_generate_algorithm.vo HMAC_DRBG_instantiate_algorithm.vo HMAC_DRBG_generate.vo HMAC_DRBG_update.vo DRBG_working_state.vo DRBG_generate_algorithm_result.vo HMAC_DRBG_reseed_algorithm.vo DRBG_reseed_function.vo HMAC256_DRBG_tests_no_reseed.vo HMAC256_DRBG_tests_pr_false.vo HMAC256_DRBG_tests_pr_true.vo hmac_drbg.vo spec_hmac_drbg.vo verif_hmac_drbg_update.vo verif_hmac_drbg_reseed.vo verif_mocked_md.vo hmac_drbg_compspecs.vo entropy.vo
GLOBFILES=$(VOFILES:.vo=.glob)

all: verifications

clean:
	rm -f $(VOFILES)
	rm -f $(GLOBFILES)

shafiles:
	(cd sha && rm -f *.vo && for i in $(SHAPATH)/*.vo; do ln -s $$i; done)

tests: HMAC256_DRBG_tests_pr_false.vo HMAC256_DRBG_tests_pr_true.vo HMAC256_DRBG_tests_no_reseed.vo

verifications: verif_hmac_drbg_update.vo verif_mocked_md.vo verif_hmac_drbg_reseed.vo

%.vo : %.v
	$(COQC) $<

verif_mocked_md.vo: hmac_drbg.vo spec_hmac_drbg.vo

verif_hmac_drbg_update.vo: spec_hmac_drbg.vo hmac_drbg.vo HMAC_DRBG_update.vo

verif_hmac_drbg_reseed.vo: spec_hmac_drbg.vo hmac_drbg.vo

hmac_drbg_compspecs.vo: hmac_drbg.vo

spec_hmac_drbg.vo: hmac_drbg.vo HMAC256_DRBG_functional_prog.vo hmac_drbg_compspecs.vo entropy.vo DRBG_reseed_function.vo DRBG_state_handle.vo

DRBG_reseed_function.vo: DRBG_state_handle.vo DRBG_working_state.vo entropy.vo

HMAC_DRBG_reseed_algorithm.vo: DRBG_working_state.vo HMAC_DRBG_update.vo

DRBG_generate_function.vo: DRBG_generate_algorithm_result.vo DRBG_working_state.vo DRBG_state_handle.vo DRBG_reseed_function.vo

DRBG_generate_algorithm_result.vo: DRBG_working_state.vo

DRBG_state_handle.vo: DRBG_working_state.vo

DRBG_instantiate_function.vo: DRBG_state_handle.vo DRBG_working_state.vo entropy.vo

HMAC256_DRBG_functional_prog.vo: DRBG_instantiate_function.vo HMAC_DRBG_instantiate_algorithm.vo DRBG_generate_function.vo HMAC_DRBG_generate_algorithm.vo DRBG_reseed_function.vo HMAC_DRBG_reseed_algorithm.vo HMAC_DRBG_update.vo

HMAC256_DRBG_tests_no_reseed.vo: HMAC256_DRBG_functional_prog.vo entropy.vo DRBG_state_handle.vo DRBG_generate_function.vo DRBG_instantiate_function.vo

HMAC256_DRBG_tests_pr_false.vo: HMAC256_DRBG_functional_prog.vo entropy.vo DRBG_state_handle.vo DRBG_generate_function.vo DRBG_instantiate_function.vo DRBG_reseed_function.vo

HMAC256_DRBG_tests_pr_true.vo: HMAC256_DRBG_functional_prog.vo entropy.vo DRBG_state_handle.vo DRBG_generate_function.vo DRBG_instantiate_function.vo DRBG_reseed_function.vo

HMAC_DRBG_generate_algorithm.vo: DRBG_working_state.vo DRBG_state_handle.vo DRBG_generate_algorithm_result.vo HMAC_DRBG_update.vo

HMAC_DRBG_instantiate_algorithm.vo: HMAC_DRBG_update.vo DRBG_working_state.vo
