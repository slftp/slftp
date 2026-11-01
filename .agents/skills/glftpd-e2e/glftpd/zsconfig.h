/* pzs-ng settings for the glftpd-e2e sites, overriding zipscript/include/zsconfig.defaults.h:
 * SFV checking and the complete/incomplete markers in every section below /site/, with no audio,
 * zip, group or sample handling. */
#define sitepath_dir                  "/site/"
#define sfv_dirs                      "/site/"
#define cleanupdirs                   "/site/"
#define zip_dirs                      ""
#define group_dirs                    ""
#define nocheck_dirs                  ""
#define check_for_missing_nfo_dirs    ""
#define check_for_missing_sample_dirs ""
#define short_sitename                "E2E"
#define debug_altlog                  FALSE
