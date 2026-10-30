# Scenario race: !spread of one release from E2E1 to E2E2. Both sites require TLS for data, so the
# FXP has to use SSL FXP. pzs-ng checks the result on both sites.
# Sourced by e2e.sh, which provides the helpers (see "Adding a scenario" in SKILL.md).

sites=2
sections=RACE

steps() {
  local rls=Test.Race.2026.1080p.WEB.h264-E2EGRP
  local src="$run_dir/release/$rls" t0

  make_release "$src" 10 5 || return
  # sfv first, as racers do
  ftp_upload 1 "/RACE/$rls" "$src" e2egrp.sfv e2egrp.nfo e2egrp.rar $(cd "$src" && echo e2egrp.r[0-9][0-9]) ||
    return
  check 'pzs-ng marks the upload on E2E1 complete' site_complete 1 "/RACE/$rls"

  add_sites || return
  console '!routeset E2E1 E2E2 5'
  console '!ruleadd * RACE if default then ALLOW'
  check 'slFtp accepts the route and the rule' console_ok
  t0=$EPOCHREALTIME
  console "!spread E2E1 RACE $rls"
  wait_console "Spreading of $rls has been" 1 300
  note "spread took $(seconds_since "$t0") s, including the 0.7 s console delay"

  check 'slFtp reports the spread as successful' \
    console_has "Spreading of $rls has been successfully finished"
  check 'E2E2 has the same files as E2E1' site_same_files 1 2 "/RACE/$rls"
  check 'pzs-ng marks the release on E2E2 complete' site_complete 2 "/RACE/$rls"
}
