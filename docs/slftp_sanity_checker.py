
import sys
import os
import re
from pathlib import Path
from typing import List, Dict, Set, Tuple, Optional
from dataclasses import dataclass, field
from enum import Enum

class Severity(Enum):
    INFO = "info"
    WARNING = "warning"
    ERROR = "error"
    CRITICAL = "critical"

@dataclass
class CheckResult:
    severity: Severity
    message: str
    file: str
    section: Optional[str] = None
    key: Optional[str] = None
    line: Optional[int] = None
    
    def __str__(self) -> str:
        location = f"[{self.file}"
        if self.line:
            location += f":{self.line}"
        if self.section:
            location += f"][{self.section}"
            if self.key:
                location += f".{self.key}"
        location += "]"
        return f"[{self.severity.value.upper()}] {location}: {self.message}"

class BaseChecker:
    def __init__(self, filepath: Path):
        self.filepath = filepath
        self.results: List[CheckResult] = []
        
    def add_result(self, severity: Severity, message: str, section: str = None, 
                   key: str = None, line: int = None):
        self.results.append(CheckResult(
            severity=severity,
            message=message,
            file=self.filepath.name,
            section=section,
            key=key,
            line=line
        ))
        
    def check(self) -> List[CheckResult]:
        raise NotImplementedError

class SlftpIniChecker(BaseChecker):
    
    KNOWN_SECTIONS = {
        "debug": {
            "debugfile": str,
            "flushlines": int,
            "categories": str,
            "verbosity": int,
            "hide_plain_text": int,
            "event_based_locking_timeout": int,
            "monitor_lock_times": int,
        },
        "http": {
            "enabled": int,
            "proxyname": str,
        },
        "api": {
            "enabled": int,
            "host": str,
            "port": int,
            "apikey": str,
        },
        "console": {
            "width": int,
            "height": int,
            "maxlines": int,
            "history_maxlines": int,
            "customtitle": str,
            "add_time_stamp": int,
            "no_console_msg": int,
            "no_console_queue": int,
            "no_console_slot": int,
            "show_uptime": int,
            "show_infos": int,
            "new_news_announce_interval": int,
        },
        "ident": {
            "enabled": int,
            "response": str,
        },
        "sites": {
            "maxidle": int,
            "idleinterval": int,
            "maxrelogins": int,
            "autologin": int,
            "socks5": int,
            "legacycwd": int,
            "delay_between_connects": int,
            "set_down_on_out_of_credits": int,
            "set_down_on_out_of_space": int,
            "killafter": int,
            "split_site_data": int,
            "admin_sitename": str,
            "admin_siteslots": int,
            "kill_ghosts_on_startup": int,
        },
        "socks5": {
            "enabled": int,
            "host": str,
            "port": str,  
            "username": str,
            "password": str,
        },
        "irc": {
            "socks5": int,
            "manglehost": int,
            "sleep_on_error": int,
            "nickname": str,
            "username": str,
            "realname": str,
            "flood": int,
            "timeout": int,
            "cmdprefix": str,
            "register_timeout": int,
            "echo_nick_change_events": int,
            "echo_topic_change_events": int,
            "echo_join_part_events": int,
            "echo_kick_events": int,
            "direct_echo": int,
            "admin_forward_msgs": int,
            "anickname": str,
        },
        "news": {
            "new_news_announce_interval": int,
        },
        "timeout": {
            "connect": int,
            "io": int,
        },
        "tags": {
            "complete_regex": str,
            "incomplete_regex": str,
        },
        "spread": {
            "announcetime": int,
        },
        "kb": {
            "kb_keep_entries": int,
            "kb_save_entries": int,
            "max_sectionhelper": int,
            "use_new_language_base": int,
            "TMP3Release": str,
            "T0dayRelease": str,
            "TNFORelease": str,
            "TIMDBRelease": str,
            "TTVRelease": str,
            "TMVIDRelease": str,
            "mp3languages": str,
            "mp3source_BD": str,
            "mp3source_CD": str,
            "mp3source_DVD": str,
            "mp3source_HDDVD": str,
            "mp3source_LIVE": str,
            "mp3source_TAPE": str,
            "mp3source_VINYL": str,
            "mp3source_WEB": str,
            "mp3source_FLASH": str,
            "mp3source_OTHER": str,
            "mp3types": str,
            "mp3genres": str,
            "tvtags": str,
            "0daysource_WIN": str,
            "0daysource_LINUX": str,
            "0daysource_UNIX": str,
            "0daysource_MAC": str,
            "0daysource_NAS": str,
            "0daysource_ANDROID": str,
            "0daysource_IOS": str,
            "enable_try_to_complete": int,
            "try_to_complete_after": int,
            "only_use_routable_sites_on_try_to_complete": int,
            "nomp3dirlistgenre": int,
            "nonfodirlistgenre": int,
            "nomvdirlistgenre": int,
            "renamed_group_checker": int,
            "trimmed_shit_checker": int,
            "renamed_release_checker": int,
            "remove_internal_tag_on_knowgroup": int,
            "remove_web_tag_on_knowgroup": int,
            "auto_add_affils": int,
            "skip_rip_older_then_one_month": int,
        },
        "fake": {
            "fake__enabled": int,
            "fake__min_release_length": int,
            "fake__few_different_chars": int,
            "fake__many_different_chars": int,
            "fake__many_dots": int,
            "fake__many_short_words_length": int,
            "fake__many_short_words_count": int,
            "fake__banned_words": str,
            "fake__many_vocal": int,
            "fake_mp3_enabled": int,
            "fake_mp3_many_dots": int,
            "fake_mp3_min_release_length": int,
            "fake_mp3_few_different_chars": int,
            "fake_mp3_many_different_chars": int,
            "fake_mp3_many_short_words_length": int,
            "fake_mp3_many_short_words_count": int,
            "fake_mp3_banned_words": str,
            "fake_mp3_many_vocal": int,
        },
        "midnight": {
            "sections": str,
            "starts": str,
            "ends": str,
        },
        "autodirlist": {
            "dropolder": int,
            "reqfill_delay": int,
            "compare_files_for_reqfilled_fallback": int,
            "only_use_routable_sites_on_reqfill": int,
            "use_site_search_on_reqfill": int,
            "fill_already_on_site": int,
            "create_already_on_site_in_directory": int,
        },
        "speedstats": {
            "save_interval": int,
            "recalc_routes_interval": int,
            "max_entries": int,
            "min_filesize": int,
            "reduced_speedstat_weight": int,
        },
        "speedtest": {
            "announce_interval": int,
            "speedtest_filename_suffix": str,
            "local_upload_mb": int,
            "min_filesize": int,
            "max_filesize": int,
            "preferred_filesize": int,
        },
        "indexer": {
            "expect_nfo_files": str,
            "database": str,
            "reqfill_delay": int,
            "transaction": int,
            "pragma": str,
            "max_deep": int,
            "use_custom_dirlist_command": int,
            "custom_dirlist_command": str,
        },
        "stats": {
            "enabled": int,
            "database": str,
            "min_filesize": int,
            "delete_after_days": int,
        },
        "backup": {
            "backup_dir": str,
            "run_backup_on_startup": int,
            "keep_backups": int,
            "backup_interval": int,
            "skipfiles": str,
        },
        "ranks": {
            "percent_of_sites_to_score": int,
            "save_interval": int,
            "recalc_ranks_interval": int,
        },
        "taskrace": {
            "newdir_max_unchanged": int,
            "newdir_max_empty": int,
            "newdir_max_created": int,
            "newdir_dirlist_readd": int,
            "badcrcevents": int,
            "convert_filenames_to_lowercase": int,
            "autoruleadd": int,
            "SFVRelease": str,
            "kill_connection_on_stalled_transfer_seconds": int,
            "auto_remove_denied_routes": int,
            "show_complete_time_stats": int,
            "wait_for_complete_subdir_types": str,
            "newdir_max_completed": int,
            "newdir_dirlist_readd_load_enabled": int,
            "newdir_dirlist_readd_load_threshold": str,
            "newdir_dirlist_readd_load_steps": str,
        },
        "taskgenredirlist": {
            "readd_attempts": int,
            "readd_interval": int,
        },
        "taskgenrenfo": {
            "readd_attempts": int,
            "readd_interval": int,
        },
        "taskmvid": {
            "readd_attempts": int,
            "readd_interval": int,
        },
        "tasknulldaynfo": {
            "readd_attempts": int,
            "readd_interval": int,
        },
        "taskgame": {
            "readd_attempts": int,
            "readd_interval": int,
        },
        "tasksitenfo": {
            "readd_attempts": int,
            "readd_interval": int,
        },
        "dbaddimdb": {
            "database": str,
            "post_lookup_infos": int,
            "skip_tv_releases": int,
            "parse_boxofficemojo_always": int,
            "update_time_in_days": int,
            "enable_boxofficemojo_lookup": int,
        },
        "taskimdb": {
            "readd_attempts": int,
            "readd_interval": int,
        },
        "tasktvinfo": {
            "database": str,
            "pragma": str,
            "days_between_last_update": int,
            "post_lookup_infos": int,
            "readd_attempts": int,
            "readd_interval": int,
            "addcmd": str,
            "max_sid_lookup_results": int,
            "stop_on_englishcheck": int,
            "use_new_announce_style": int,
        },
        "taskidle": {
            "idlecommands": str,
        },
        "queue": {
            "enable_queueclean": int,
            "queueclean_interval": int,
            "queueclean_unassigned": int,
            "queueclean_maxrunning": int,
            "maxassign": int,
            "maxassign_delay": int,
            "queue_fire": int,
            "sample_dirs_priority": int,
            "proof_dirs_priority": int,
            "cover_dirs_priority": int,
            "subs_dirs_priority": int,
            "image_files_priority": int,
            "video_files_priority": int,
            "nfo_files_priority": int,
            "sfv_files_priority": int,
        },
        "taskpretime": {
            "mode": int,
            "mode_2": int,
            "readd_attempts": int,
            "readd_interval": int,
            "default_pretime": int,
            "offset": int,
            "url": str,
        },
        "dirlist": {
            "global_skip_files": str,
            "global_skip_dirs": str,
            "skip_being_uploaded_files": int,
        },
        "precatcher": {
            "recursiv_mapping": int,
            "precatcher_debug": int,
            "debugfile": str,
        },
        "dbaddpre": {
            "addprecmd": str,
            "addpreechocmd": str,
            "database": str,
            "mode": int,
            "add_to_kb_on_dbaddpre_insert": int,
            "sightings_threshold": int,
            "db_file": str,
        },
        "mysql": {
            "host": str,
            "port": int,
            "user": str,
            "pass": str,
            "dbname": str,
            "dbms": str,
        },
        "taskmysqlpretime": {
            "tablename": str,
            "rlsname_field": str,
            "section_field": str,
            "rlsdate_field": str,
            "source_field": str,
        },
        "prebot": {
            "predir_re_examine_time": int,
        },
        "UDPConfig": {
            "EnableUDP": str,
            "IP": str,
            "Port": int,
            "ApiPort": int,
            "Password": str,
            "EncryptUDP": int,
        },
        "performance_monitor": {
            "enabled": int,
            "cpu_threshold_high": int,
            "cpu_threshold_low": int,
        },
        "loadmonitor": {
            "profile_report_interval": int,
        },
        "eprecatcher": {
            "enabled": int,
            "bindhost": str,
            "bindport": int,
        },
    }
    
    
    REQUIRED_SECTIONS = {"sites", "irc", "kb"}
    
    
    VALUE_RANGES = {
        ("debug", "verbosity"): (0, 3),
        ("timeout", "connect"): (1, 300),
        ("timeout", "io"): (1, 300),
        ("queue", "queue_fire"): (1, 1000),
        ("taskpretime", "mode"): (0, 3),
        ("taskpretime", "mode_2"): (0, 3),
        ("dbaddpre", "mode"): (0, 2),
    }
    
    def check(self) -> List[CheckResult]:
        if not self.filepath.exists():
            self.add_result(Severity.CRITICAL, f"File not found: {self.filepath}")
            return self.results
            
        content = self.filepath.read_text(encoding='utf-8', errors='ignore')
        lines = content.split('\n')
        
        current_section = None
        seen_sections = set()
        section_values = {}  

        for line_num, line in enumerate(lines, 1):
            stripped = line.strip()

            
            if not stripped or stripped.startswith('#') or stripped.startswith(';'):
                continue

            
            if stripped.startswith('[') and stripped.endswith(']'):
                current_section = stripped[1:-1].strip()
                seen_sections.add(current_section)

                
                if current_section not in self.KNOWN_SECTIONS:
                    self.add_result(Severity.WARNING, f"Unknown section: {current_section}",
                                  current_section, line=line_num)
                continue

            
            if '=' in stripped and current_section is not None:
                if current_section in self.KNOWN_SECTIONS:
                    self._check_key_value(current_section, stripped, line_num)
                    
                    key, _, value = stripped.partition('=')
                    key = key.strip()
                    value = self._strip_comment(value.strip())
                    section_values[(current_section, key)] = value

        
        for req_section in self.REQUIRED_SECTIONS:
            if req_section not in seen_sections:
                self.add_result(Severity.ERROR, f"Required section missing: {req_section}")

        
        self._check_api_key(content)
        self._check_regex_patterns(content)

        
        self._check_conditional_values(section_values)

        return self.results
        
    def _strip_comment(self, value: str) -> str:
        
        result = []
        in_quotes = False
        quote_char = None
        
        for i, char in enumerate(value):
            if char in ('"', "'") and (i == 0 or value[i-1] != '\\'):
                if not in_quotes:
                    in_quotes = True
                    quote_char = char
                elif char == quote_char:
                    in_quotes = False
                    quote_char = None
            elif not in_quotes and char in ('#', ';'):
                break
            result.append(char)
            
        return ''.join(result).strip()
        
    def _check_key_value(self, section: str, line: str, line_num: int):
        if '=' not in line:
            return
            
        key, _, value = line.partition('=')
        key = key.strip()
        value = self._strip_comment(value.strip())
        
        known_keys = self.KNOWN_SECTIONS.get(section, {})
        
        if key not in known_keys:
            self.add_result(Severity.INFO, f"Unknown key: {key}", 
                          section, key, line_num)
            return
            
        expected_type = known_keys[key]
        
        
        if expected_type == int:
            try:
                int_value = int(value)
                
                if (section, key) in self.VALUE_RANGES:
                    min_val, max_val = self.VALUE_RANGES[(section, key)]
                    if int_value < min_val or int_value > max_val:
                        self.add_result(Severity.WARNING, 
                            f"Value outside recommended range [{min_val}-{max_val}]: {int_value}",
                            section, key, line_num)
            except ValueError:
                self.add_result(Severity.ERROR, 
                    f"Integer expected, but '{value}' found",
                    section, key, line_num)
                    
    def _check_conditional_values(self, section_values: dict):
        
        socks5_enabled = section_values.get(('socks5', 'enabled'), '0')
        port = section_values.get(('socks5', 'port'), '')
        if socks5_enabled == '1':
            if port == '':
                self.add_result(Severity.ERROR,
                    "socks5 is enabled but port is empty",
                    'socks5', 'port')
            else:
                try:
                    port_int = int(port)
                    if port_int < 1 or port_int > 65535:
                        self.add_result(Severity.ERROR,
                            f"Invalid port range: {port_int}",
                            'socks5', 'port')
                except ValueError:
                    self.add_result(Severity.ERROR,
                        f"Integer expected, but '{port}' found",
                        'socks5', 'port')

    def _check_api_key(self, content: str):
        
        
        
        KNOWN_DEFAULTS = {'', '555nase'}
        if '[api]' in content:
            import re
            apikey_match = re.search(r'apikey\s*=\s*(.*)', content)
            if apikey_match:
                apikey = apikey_match.group(1).strip()
                if apikey in KNOWN_DEFAULTS:
                    self.add_result(Severity.WARNING,
                        f"API key uses default value '{apikey}' - please change!")
                    
    def _check_regex_patterns(self, content: str):
        import re
        
        regex_keys = [
            ('tags', 'complete_regex'),
            ('tags', 'incomplete_regex'),
            ('dirlist', 'global_skip_files'),
            ('dirlist', 'global_skip_dirs'),
        ]
        
        for section, key in regex_keys:
            pattern = re.search(rf'\[{section}\].*?{key}\s*=\s*(.+?)(?:\n\[|\Z)', 
                              content, re.DOTALL)
            if pattern:
                regex_value = pattern.group(1).strip().replace('\n', '').replace('\r', '')
                try:
                    re.compile(regex_value)
                except re.error as e:
                    self.add_result(Severity.ERROR, 
                        f"Invalid regex in {key}: {e}",
                        section, key)

class SitesDatChecker(BaseChecker):
    
    SITE_KEYS = {
        'username', 'password', 'slots', 'max_dn', 'max_pre_dn', 'max_up',
        'ircnick', 'bnc_host-0', 'bnc_port-0', 'proxyname', 'sslmethod',
        'sslfxp', 'legacydirlist', 'noannounce', 'sw', 'swVersion',
        'sectiondir', 'sectionprecmd', 'siteaffils', 'sectionpretime',
        'autobnctest', 'autonuke', 'autorules', 'autodirlist', 'autoindex',
        'maxidle', 'idleinterval', 'connect_timeout', 'io_timeout',
        'country', 'ident', 'sitefullname', 'sitelinkspeed', 'sitesize',
        'sitenotes', 'usefornfodownload', 'skipbeinguploadedfiles',
        'permdown', 'skippre', 'siteinfos', 'useautoinvite',
        'setdownonoutofspace', 'setdownonoutofcredits',
        'usereversefxpsource', 'usereversefxpdestination',
        'usesitesearchonreqfill', 'reducedspeedstatweight',
        'killconnectiononstalledtransferseconds', 'maxupperrip',
    }
    
    
    IRCNET_KEYS = {
        'ssl', 'password', 'nick', 'anick', 'ident', 'username',
        'bnc_host-0', 'bnc_port-0',
    }
    
    
    CHANNEL_KEYS = {
        'blowkey', 'cbc', 'names', 'chankey',
    }
    
    
    SPEEDFROM_KEYS = set()  
    
    
    INTEGER_KEYS = {
        'slots', 'max_dn', 'max_pre_dn', 'max_up', 'bnc_port-0',
        'sslmethod', 'sslfxp', 'noannounce', 'legacydirlist', 'autobnctest',
        'autonuke', 'autorules', 'autodirlist', 'autoindex', 'maxidle',
        'idleinterval', 'connect_timeout', 'io_timeout', 'sectionpretime',
        'usefornfodownload', 'skipbeinguploadedfiles', 'permdown', 'skippre',
        'useautoinvite', 'setdownonoutofspace', 'setdownonoutofcredits',
        'usereversefxpsource', 'usereversefxpdestination',
        'usesitesearchonreqfill', 'reducedspeedstatweight',
        'killconnectiononstalledtransferseconds', 'maxupperrip',
        'ssl', 'cbc',
    }
    
    
    SSL_METHODS = {0, 1, 2, 3}  
    
    def check(self) -> List[CheckResult]:
        if not self.filepath.exists():
            self.add_result(Severity.CRITICAL, f"File not found: {self.filepath}")
            return self.results
            
        content = self.filepath.read_text(encoding='utf-8', errors='ignore')
        lines = content.split('\n')
        
        current_section = None
        section_type = None
        sites = []
        ircnets = []
        channels = []
        
        bnc_map = {}  
        cred_map = {}  
        section_data = {}  

        for line_num, line in enumerate(lines, 1):
            stripped = line.strip()

            if not stripped or stripped.startswith('#') or stripped.startswith(';'):
                continue

            
            if stripped.startswith('[') and stripped.endswith(']'):
                current_section = stripped[1:-1].strip()
                section_type = self._get_section_type(current_section)
                section_data[current_section] = {}

                if section_type == 'site':
                    sites.append(current_section)
                elif section_type == 'ircnet':
                    ircnets.append(current_section)
                elif section_type == 'channel':
                    channels.append(current_section)

                
                if section_type == 'unknown' and current_section not in ['sites', 'precatcher']:
                    self.add_result(Severity.WARNING,
                        f"Unknown section: {current_section}",
                        current_section, line=line_num)
                continue

            
            if '=' in stripped and current_section is not None:
                self._check_key_value(current_section, section_type, stripped, line_num)
                key, _, value = stripped.partition('=')
                key = key.strip()
                value = self._strip_comment(value.strip())
                section_data[current_section][key] = (value, line_num)

        
        self._check_duplicate_names(sites, 'Site')
        self._check_duplicate_names(ircnets, 'IRC-Netzwerk')
        self._check_duplicate_names(channels, 'Channel')

        
        

        
        self._check_sites_completeness(sites, content)
        self._check_ircnets_completeness(ircnets, content)
        self._check_channels_completeness(channels, content)

        return self.results
        
    def _get_section_type(self, section: str) -> str:
        if section.startswith('site-'):
            return 'site'
        elif section.startswith('ircnet-'):
            return 'ircnet'
        elif section.startswith('channel-'):
            return 'channel'
        elif section.startswith('speed-from-'):
            return 'speed-from'
        elif section in ['sites', 'precatcher']:
            return 'system'
        return 'unknown'
        
    def _strip_comment(self, value: str) -> str:
        result = []
        in_quotes = False
        quote_char = None
        
        for i, char in enumerate(value):
            if char in ('"', "'") and (i == 0 or value[i-1] != '\\'):
                if not in_quotes:
                    in_quotes = True
                    quote_char = char
                elif char == quote_char:
                    in_quotes = False
                    quote_char = None
            elif not in_quotes and char in ('#', ';'):
                break
            result.append(char)
            
        return ''.join(result).strip()
        
    def _check_key_value(self, section: str, section_type: str, line: str, line_num: int):
        key, _, value = line.partition('=')
        key = key.strip()
        value = self._strip_comment(value.strip())
        
        
        if section_type == 'site':
            
            if not (key.startswith('rank-') or key.startswith('bnc_host-') or 
                    key.startswith('bnc_port-') or key.startswith('delayleech-') or
                    key.startswith('delayupload-') or key.startswith('speed-from-') or
                    key in self.SITE_KEYS):
                self.add_result(Severity.INFO, f"Possibly unknown key: {key}",
                              section, key, line_num)
                              
        elif section_type == 'ircnet':
            if not (key.startswith('bnc_host-') or key.startswith('bnc_port-') or
                    key in self.IRCNET_KEYS):
                self.add_result(Severity.INFO, f"Possibly unknown key: {key}",
                              section, key, line_num)
                              
        elif section_type == 'channel':
            if key not in self.CHANNEL_KEYS:
                self.add_result(Severity.INFO, f"Possibly unknown key: {key}",
                              section, key, line_num)
                              
        
        if key in self.INTEGER_KEYS:
            try:
                int(value)
            except ValueError:
                self.add_result(Severity.ERROR, 
                    f"Integer expected, but '{value}' found",
                    section, key, line_num)
                    
        
        if section_type == 'site':
            if key == 'sslmethod':
                try:
                    method = int(value)
                    if method not in self.SSL_METHODS:
                        self.add_result(Severity.WARNING,
                            f"Invalid sslmethod value: {method} (valid: 0-3)",
                            section, key, line_num)
                except ValueError:
                    pass
                    
        
        if 'port' in key.lower():
            try:
                port = int(value)
                if port < 1 or port > 65535:
                    self.add_result(Severity.ERROR,
                        f"Invalid port range: {port}",
                        section, key, line_num)
            except ValueError:
                pass
                
    def _check_sites_completeness(self, sites: List[str], content: str):
        for site in sites:
            if f"[{site}]" not in content:
                continue
            section_start = content.find(f"[{site}]")
            section_end = content.find("[", section_start + 1)
            if section_end == -1:
                section_end = len(content)
            section_content = content[section_start:section_end]

            required = ['username', 'password', 'bnc_host-0', 'bnc_port-0']
            for req in required:
                if f"\n{req}=" not in section_content and not section_content.startswith(f"{req}="):
                    self.add_result(Severity.ERROR,
                        f"Site has no '{req}' set", site)

            
            if 'slots=' not in section_content:
                self.add_result(Severity.WARNING,
                    f"Site has no 'slots' configured", site)

    def _check_ircnets_completeness(self, ircnets: List[str], content: str):
        for ircnet in ircnets:
            if f"[{ircnet}]" not in content:
                continue
            section_start = content.find(f"[{ircnet}]")
            section_end = content.find("[", section_start + 1)
            if section_end == -1:
                section_end = len(content)
            section_content = content[section_start:section_end]

            if 'nick=' not in section_content:
                self.add_result(Severity.WARNING,
                    f"IRC network has no nick set", ircnet)

            
            if 'bnc_host-0=' not in section_content:
                self.add_result(Severity.WARNING,
                    f"IRC network has no bnc_host-0 set", ircnet)
            if 'bnc_port-0=' not in section_content:
                self.add_result(Severity.WARNING,
                    f"IRC network has no bnc_port-0 set", ircnet)
                        
    def _check_channels_completeness(self, channels: List[str], content: str):
        for channel in channels:
            if f"[{channel}]" in content:
                section_start = content.find(f"[{channel}]")
                section_end = content.find("[", section_start + 1)
                if section_end == -1:
                    section_end = len(content)
                section_content = content[section_start:section_end]
                
                
                if 'cbc=1' in section_content and 'blowkey=' not in section_content:
                    self.add_result(Severity.WARNING,
                        f"Channel uses CBC but has no blowkey", channel)

    def _check_duplicate_names(self, names: List[str], label: str):
        seen = {}
        for name in names:
            lower_name = name.lower()
            if lower_name in seen:
                self.add_result(Severity.ERROR,
                    f"Duplicate {label} name: {name}")
            else:
                seen[lower_name] = name

class SkipFileChecker(BaseChecker):
    VALID_KEYS = {
        'allowedfiles', 'alloweddirs', 'dirdepth',
        'skipfiles_dn', 'skipfiles_up', 'skipdirs_up', 'skipdirs_dn',
    }
    
    def check(self) -> List[CheckResult]:
        if not self.filepath.exists():
            self.add_result(Severity.INFO, f"File not found: {self.filepath}")
            return self.results
            
        content = self.filepath.read_text(encoding='utf-8', errors='ignore')
        lines = content.split('\n')
        
        current_section = None
        
        for line_num, line in enumerate(lines, 1):
            stripped = line.strip()
            
            if not stripped or stripped.startswith('#'):
                continue
                
            
            if stripped.startswith('[') and stripped.endswith(']'):
                current_section = stripped[1:-1].strip()
                
                
                if not (current_section == 'skiplist' or 
                        current_section.startswith('skiplist-')):
                    self.add_result(Severity.WARNING,
                        f"Unusual section name: {current_section}",
                        current_section, line=line_num)
                continue
                
            
            if '=' in stripped:
                key, _, value = stripped.partition('=')
                key = key.strip()
                
                for comment_char in ('#', ';'):
                    if comment_char in value:
                        value = value[:value.index(comment_char)]
                value = value.strip()
                
                if key not in self.VALID_KEYS:
                    self.add_result(Severity.INFO,
                        f"Unknown key: {key}",
                        current_section, key, line_num)
                        
                
                if key in ('allowedfiles', 'alloweddirs'):
                    if ':' not in value:
                        self.add_result(Severity.ERROR,
                            f"Format should be: directory:masks (e.g. _ROOT_:*.mp3)",
                            current_section, key, line_num)
                            
                
                if key == 'dirdepth':
                    try:
                        depth = int(value)
                        if depth < 0 or depth > 10:
                            self.add_result(Severity.WARNING,
                                f"Unusual dirdepth value: {depth}",
                                current_section, key, line_num)
                    except ValueError:
                        self.add_result(Severity.ERROR,
                            f"Integer expected, but '{value}' found",
                            current_section, key, line_num)
                            
        return self.results

class RtplChecker(BaseChecker):
    VALID_ACTIONS = {'ALLOW', 'ACCEPT', 'DROP'}

    def check(self) -> List[CheckResult]:
        if not self.filepath.exists():
            self.add_result(Severity.INFO, f"File not found: {self.filepath}")
            return self.results

        content = self.filepath.read_text(encoding='utf-8', errors='ignore')
        lines = content.split('\n')
        rules = []  
        seen_rules = {}  
        default_seen = {}  

        for line_num, line in enumerate(lines, 1):
            stripped = line.strip()
            if not stripped or stripped.startswith('#') or stripped.startswith(';'):
                continue

            
            if ' if ' not in stripped or ' then ' not in stripped:
                self.add_result(Severity.ERROR,
                    f"Invalid rule syntax (expected: '... if ... then ...'): {stripped[:60]}",
                    line=line_num)
                continue

            condition_part, _, action_part = stripped.partition(' then ')
            action = action_part.strip().upper()
            if action not in self.VALID_ACTIONS:
                self.add_result(Severity.ERROR,
                    f"Invalid action '{action_part.strip()}' (expected: ALLOW or DROP)",
                    line=line_num)
                continue

            
            prefix = condition_part.strip()
            
            if ' ifnot ' in prefix:
                site_section = prefix.split(' ifnot ')[0].strip()
                condition = prefix[len(site_section):].strip()
                if condition.startswith('ifnot '):
                    condition = condition[6:].strip()
                condition = 'not ( ' + condition + ' )'
            elif ' if ' in prefix:
                site_section = prefix.split(' if ')[0].strip()
                condition = prefix[len(site_section):].strip()
                if condition.startswith('if '):
                    condition = condition[3:].strip()
            else:
                site_section = ''
                condition = prefix
            rules.append((line_num, stripped, site_section, condition, action))

            
            rule_key = (site_section.lower(), condition.lower())
            if rule_key in seen_rules:
                prev_line, prev_action = seen_rules[rule_key]
                if prev_action == action:
                    self.add_result(Severity.WARNING,
                        f"Duplicate rule (also line {prev_line}): {condition[:60]}",
                        line=line_num)
                else:
                    self.add_result(Severity.ERROR,
                        f"Contradictory rule at line {prev_line} ({prev_action} vs {action}): {condition[:60]}",
                        line=line_num)
            else:
                seen_rules[rule_key] = (line_num, action)

            
            if condition.lower() == 'default':
                ss_lower = site_section.lower()
                if ss_lower in default_seen:
                    self.add_result(Severity.WARNING,
                        f"Multiple default rules for '{site_section}' (first at line {default_seen[ss_lower]})",
                        line=line_num)
                default_seen[ss_lower] = line_num

            
            self._check_regex_in_condition(condition, line_num)

        
        

        return self.results

    def _check_regex_in_condition(self, condition: str, line_num: int):
        
        import re as re_mod
        
        patterns = re_mod.findall(r'/(.+?)/([i]?)', condition)
        for pattern, flags in patterns:
            try:
                re_mod.compile(pattern, re_mod.IGNORECASE if 'i' in flags else 0)
            except re_mod.error as e:
                self.add_result(Severity.ERROR,
                    f"Invalid regex '/{pattern}/': {e}",
                    line=line_num)

class SettingsChecker(BaseChecker):
    def check(self) -> List[CheckResult]:
        if not self.filepath.exists():
            self.add_result(Severity.INFO, f"File not found: {self.filepath}")
            return self.results

        content = self.filepath.read_text(encoding='utf-8', errors='ignore')
        lines = content.split('\n')

        sections = []
        dir_mappings = {}  
        seen_keys = {}  

        for line_num, line in enumerate(lines, 1):
            stripped = line.strip()
            if not stripped or stripped.startswith('#') or stripped.startswith(';'):
                continue

            if '=' not in stripped:
                continue

            key, _, value = stripped.partition('=')
            key = key.strip()
            value = value.strip()

            
            if key in seen_keys and not key.startswith('dir-'):
                self.add_result(Severity.WARNING,
                    f"Duplicate key '{key}' (first definition at line {seen_keys[key]})",
                    line=line_num)
            else:
                seen_keys[key] = line_num

            if key == 'sections':
                sections = [s.strip() for s in value.split() if s.strip()]
                
                seen_secs = set()
                for sec in sections:
                    if sec in seen_secs:
                        self.add_result(Severity.WARNING,
                            f"Duplicate section in sections list: '{sec}'",
                            line=line_num)
                    seen_secs.add(sec)

            elif key.startswith('dir-'):
                section = key[4:]
                if not value:
                    self.add_result(Severity.ERROR,
                        f"Empty path for dir-{section}",
                        line=line_num)
                if section not in dir_mappings:
                    dir_mappings[section] = []
                
                for prev_line, prev_path in dir_mappings[section]:
                    if prev_path == value:
                        self.add_result(Severity.WARNING,
                            f"Duplicate dir-{section}='{value}' (first definition at line {prev_line})",
                            line=line_num)
                dir_mappings[section].append((line_num, value))

            elif key == 'affils':
                
                affils = [a.strip() for a in value.split() if a.strip()]
                seen_affils = set()
                for aff in affils:
                    if aff in seen_affils:
                        self.add_result(Severity.WARNING,
                            f"Duplicate affil: '{aff}'",
                            line=line_num)
                    seen_affils.add(aff)

        
        sections_set = set(sections)
        dir_sections_set = set(dir_mappings.keys())

        for sec in sections_set:
            if sec not in dir_sections_set:
                self.add_result(Severity.WARNING,
                    f"Section '{sec}' in sections= but no dir-{sec}= defined")

        for sec in dir_sections_set:
            if sec not in sections_set:
                self.add_result(Severity.WARNING,
                    f"dir-{sec}= defined but Section '{sec}' not in sections=")

        
        for sec, mappings in dir_mappings.items():
            paths = [p for _, p in mappings]
            unique_paths = set(paths)
            if len(unique_paths) > 1:
                self.add_result(Severity.INFO,
                    f"dir-{sec} has multiple different paths: {', '.join(unique_paths)}")

        return self.results

class ChansChecker(BaseChecker):
    VALID_EVENTS = {'NEWDIR', 'PRE', 'SPREAD', 'COMPLETE', 'REQUEST', 'NUKE',
                    'ADDPRE', 'UPDATE'}

    def check(self) -> List[CheckResult]:
        if not self.filepath.exists():
            self.add_result(Severity.INFO, f"File not found: {self.filepath}")
            return self.results

        content = self.filepath.read_text(encoding='utf-8', errors='ignore')
        lines = content.split('\n')
        seen_entries = set()

        for line_num, line in enumerate(lines, 1):
            stripped = line.strip()
            if not stripped or stripped.startswith('#') or stripped.startswith(';'):
                continue

            parts = stripped.split(';')
            if len(parts) < 6:
                self.add_result(Severity.ERROR,
                    f"Invalid format (expected at least 6 fields separated by ';', gefunden {len(parts)}): {stripped[:60]}",
                    line=line_num)
                continue

            site = parts[0].strip()
            channel = parts[1].strip()
            ircnet = parts[2].strip()
            site2 = parts[3].strip()
            event = parts[4].strip()
            words = parts[5].strip()
            forced_section = parts[6].strip() if len(parts) > 6 else ''

            
            entry_key = (site.lower(), channel.lower(), ircnet.lower(), site2.lower(),
                         event.lower(), words.lower())
            if entry_key in seen_entries:
                self.add_result(Severity.WARNING,
                    f"Duplicate entry: {stripped[:60]}",
                    line=line_num)
            else:
                seen_entries.add(entry_key)

            
            if event.upper() not in self.VALID_EVENTS:
                self.add_result(Severity.INFO,
                    f"Unbekanntes Event: '{event}'",
                    line=line_num)

            
            if channel and not channel.startswith('#'):
                self.add_result(Severity.WARNING,
                    f"Channel should start with #: '{channel}'",
                    line=line_num)

        return self.results

def cross_check_sites_rtpl(sites_path: Path, rtpl_path: Path, all_results: List[CheckResult], base_path: Path):
    if not sites_path.exists() or not rtpl_path.exists():
        return

    content = sites_path.read_text(encoding='utf-8', errors='ignore')
    
    sites_in_dat = set()
    for line in content.split('\n'):
        line = line.strip()
        if line.startswith('[site-') and line.endswith(']'):
            site_name = line[6:-1]  
            sites_in_dat.add(site_name.upper())

    
    admin_site = ''
    ini_path = base_path / 'slftp.ini'
    if ini_path.exists():
        ini_content = ini_path.read_text(encoding='utf-8', errors='ignore')
        import re
        m = re.search(r'^admin_sitename\s*=\s*(.+)$', ini_content, re.MULTILINE)
        if m:
            admin_site = m.group(1).strip().upper()

    
    rtpl_files = {f.stem.upper() for f in rtpl_path.glob('*.rtpl') if f.stem}
    settings_files = {f.stem.upper() for f in rtpl_path.glob('*.settings') if f.stem}
    chans_files = {f.stem.upper() for f in rtpl_path.glob('*.chans') if f.stem}

    
    for site in sorted(sites_in_dat):
        if site not in rtpl_files:
            all_results.append(CheckResult(
                severity=Severity.WARNING,
                message=f"Site '{site}' has no .rtpl file",
                file='sites.dat'
            ))
        if site not in settings_files:
            all_results.append(CheckResult(
                severity=Severity.INFO,
                message=f"Site '{site}' has no .settings file",
                file='sites.dat'
            ))
        if site not in chans_files:
            all_results.append(CheckResult(
                severity=Severity.INFO,
                message=f"Site '{site}' has no .chans file",
                file='sites.dat'
            ))

    
    for rtpl_name in sorted(rtpl_files):
        if rtpl_name not in sites_in_dat and rtpl_name != admin_site:
            all_results.append(CheckResult(
                severity=Severity.WARNING,
                message=f".rtpl file '{rtpl_name}' exists but site not in sites.dat",
                file='rtpl'
            ))

def is_encrypted_sites_dat(filepath: Path) -> bool:
    if not filepath.exists():
        return False
    try:
        with open(filepath, 'rb') as f:
            header = f.read(20)
        if not header:
            return False
        try:
            header.decode('utf-8')
            return not header.startswith(b'[')
        except UnicodeDecodeError:
            return True
    except Exception:
        return False


def find_sites_dat(base_path: Path) -> Tuple[Optional[Path], bool]:
    candidates = [
        base_path / 'sites.dat.decrypt',
        base_path / 'sites.dat',
        base_path / 'sites.dat.bak',
        base_path / 'sites.dat.backup',
    ]
    for path in candidates:
        if path.exists():
            encrypted = is_encrypted_sites_dat(path)
            return path, encrypted
    return None, False


def decrypt_sites_dat(base_path: Path, input_file: Path) -> Optional[Path]:
    passphrase_file = base_path / 'masterpass.txt'
    if not passphrase_file.exists():
        passphrase_file = base_path / '.masterpass'
    if not passphrase_file.exists():
        for f in base_path.iterdir():
            if 'pass' in f.name.lower() and f.is_file():
                passphrase_file = f
                break
    if not passphrase_file.exists():
        print(f"     No passphrase file found (expected: masterpass.txt)")
        return None
    slftp_bin = base_path / 'slftp'
    if not slftp_bin.exists():
        print(f"     slftp binary not found at: {slftp_bin}")
        return None
    output_file = base_path / 'sites.dat.decrypt'
    cmd = [str(slftp_bin), '-d', f'--pf={passphrase_file.name}', f'--infile={input_file.name}', f'--outfile={output_file.name}']
    import subprocess
    try:
        result = subprocess.run(cmd, cwd=str(base_path), capture_output=True, text=True, timeout=30)
        if result.returncode == 0 and output_file.exists():
            return output_file
        if result.stderr:
            print(f"     Decrypt error: {result.stderr.strip()}")
        elif result.stdout:
            print(f"     Decrypt output: {result.stdout.strip()}")
        else:
            print(f"     Decrypt failed with exit code {result.returncode}")
    except Exception as e:
        print(f"     Decrypt exception: {e}")
    return None


def print_results(results: List[CheckResult], verbose: bool = False):
    if not results:
        print("  ✓ No problems found!")
        return
        
    
    by_severity = {Severity.CRITICAL: [], Severity.ERROR: [], 
                   Severity.WARNING: [], Severity.INFO: []}
    
    for r in results:
        by_severity[r.severity].append(r)
        
    
    colors = {
        Severity.CRITICAL: '\033[91m',  
        Severity.ERROR: '\033[91m',      
        Severity.WARNING: '\033[93m',    
        Severity.INFO: '\033[94m',       
    }
    reset = '\033[0m'
    
    for severity in [Severity.CRITICAL, Severity.ERROR, Severity.WARNING, Severity.INFO]:
        items = by_severity[severity]
        if not items:
            continue
            
        color = colors.get(severity, '')
        print(f"\n  {color}[{severity.value.upper()}]{reset} ({len(items)}):")
        
        for r in items:
            location = ""
            if r.line:
                location += f" Zeile {r.line}"
            if r.section:
                location += f" [{r.section}"
                if r.key:
                    location += f".{r.key}"
                location += "]"
            print(f"    - {r.message}{location}")

def main():
    import argparse
    
    parser = argparse.ArgumentParser(
        description='SLFTP Sanity Checker - Checks SLFTP configuration files. '
                    'Run this from your SLFTP directory (where slftp.ini, sites.dat, etc. are located).')
    parser.add_argument('path', nargs='?', default='.',
                       help='Path to SLFTP directory (default: current directory)')
    parser.add_argument('-v', '--verbose', action='store_true',
                       help='Verbose output')
    parser.add_argument('--no-ini', action='store_true',
                       help='slftp.ini do not check')
    parser.add_argument('--no-sites', action='store_true',
                       help='sites.dat do not check')
    parser.add_argument('--no-skip', action='store_true',
                       help='slftp.skip do not check')
    parser.add_argument('--no-rtpl', action='store_true',
                       help='.rtpl files do not check')
    parser.add_argument('--no-settings', action='store_true',
                       help='.settings files do not check')
    parser.add_argument('--no-chans', action='store_true',
                       help='.chans files do not check')
    parser.add_argument('--auto-decrypt', action='store_true',
                       help='Automatically try to decrypt sites.dat if encrypted')
    
    args = parser.parse_args()
    
    base_path = Path(args.path).resolve()
    
    if not base_path.exists():
        print(f"Error: Path not found: {base_path}")
        sys.exit(1)
        
    print(f"SLFTP Sanity Checker")
    print(f"=" * 60)
    print(f"Check directory: {base_path.absolute()}")
    print()
    
    all_results = []
    sites_path = None
    
    if not args.no_ini:
        ini_path = base_path / 'slftp.ini'
        print(f"📄 Check slftp.ini...")
        if ini_path.exists():
            checker = SlftpIniChecker(ini_path)
            results = checker.check()
            all_results.extend(results)
            print_results(results, args.verbose)
        else:
            print(f"  ⚠ File not found")
        print()
    
    if not args.no_sites:
        sites_path, is_encrypted = find_sites_dat(base_path)
        
        print(f"📄 Check sites.dat...")
        if sites_path is None:
            print(f"  ⚠ File not found")
            print(f"     Expected: sites.dat.decrypt (preferred) or sites.dat")
            print(f"     Place your sites.dat in: {base_path.absolute()}")
            print()
        elif is_encrypted:
            print(f"  ⚠ File appears to be encrypted: {sites_path.name}")
            print(f"     Decryption required before checking.")
            print(f"")
            print(f"     Manual decryption:")
            print(f"       1. Ensure a passphrase file exists (e.g. masterpass.txt)")
            print(f"       2. Run: ./slftp -d --pf=masterpass.txt --infile=sites.dat --outfile=sites.dat.decrypt")
            print(f"       3. Re-run this checker")
            print(f"")
            if args.auto_decrypt:
                decrypted = decrypt_sites_dat(base_path, sites_path)
                if decrypted and decrypted.exists():
                    print(f"     Auto-decrypted to: {decrypted.name}")
                    sites_path = decrypted
                    is_encrypted = False
                else:
                    print(f"     Auto-decrypt failed. Please decrypt manually.")
                    print()
                    sites_path = None
            else:
                print(f"     Use --auto-decrypt to attempt automatic decryption.")
                print()
                sites_path = None
        
        if sites_path is not None and not is_encrypted:
            checker = SitesDatChecker(sites_path)
            results = checker.check()
            all_results.extend(results)
            print_results(results, args.verbose)
        print()
        
    
    if not args.no_skip:
        skip_path = base_path / 'slftp.skip'
        print(f"📄 Check slftp.skip...")
        if skip_path.exists():
            checker = SkipFileChecker(skip_path)
            results = checker.check()
            all_results.extend(results)
            print_results(results, args.verbose)
        else:
            print(f"  ⚠ File not found")
        print()

    
    if not args.no_rtpl:
        rtpl_path = base_path / 'rtpl'
        if rtpl_path.exists() and rtpl_path.is_dir():
            rtpl_files = sorted(rtpl_path.glob('*.rtpl'))
            if rtpl_files:
                print(f"📄 Check {len(rtpl_files)} .rtpl files...")
                for rtpl_file in rtpl_files:
                    checker = RtplChecker(rtpl_file)
                    results = checker.check()
                    if results:
                        print(f"\n  {rtpl_file.name}:")
                        print_results(results, args.verbose)
                        all_results.extend(results)
                if not any(r.file.endswith('.rtpl') for r in all_results):
                    print("  ✓ All .rtpl files OK")
                print()
            else:
                print(f"📄 Check .rtpl files...")
                print(f"  ⚠ No .rtpl files found")
                print()

    
    if not args.no_settings:
        rtpl_path = base_path / 'rtpl'
        if rtpl_path.exists() and rtpl_path.is_dir():
            settings_files = sorted(rtpl_path.glob('*.settings'))
            if settings_files:
                print(f"📄 Check {len(settings_files)} .settings files...")
                for settings_file in settings_files:
                    checker = SettingsChecker(settings_file)
                    results = checker.check()
                    if results:
                        print(f"\n  {settings_file.name}:")
                        print_results(results, args.verbose)
                        all_results.extend(results)
                if not any(r.file.endswith('.settings') for r in all_results):
                    print("  ✓ All .settings files OK")
                print()
            else:
                print(f"📄 Check .settings files...")
                print(f"  ⚠ No .settings files found")
                print()

    
    if not args.no_chans:
        rtpl_path = base_path / 'rtpl'
        if rtpl_path.exists() and rtpl_path.is_dir():
            chans_files = sorted(rtpl_path.glob('*.chans'))
            if chans_files:
                print(f"📄 Check {len(chans_files)} .chans files...")
                for chans_file in chans_files:
                    checker = ChansChecker(chans_file)
                    results = checker.check()
                    if results:
                        print(f"\n  {chans_file.name}:")
                        print_results(results, args.verbose)
                        all_results.extend(results)
                if not any(r.file.endswith('.chans') for r in all_results):
                    print("  ✓ All .chans files OK")
                print()
            else:
                print(f"📄 Check .chans files...")
                print(f"  ⚠ No .chans files found")
                print()

    
    rtpl_path = base_path / 'rtpl'
    if sites_path is not None and sites_path.exists() and rtpl_path.exists() and rtpl_path.is_dir():
        print(f"📄 Cross-check sites.dat vs rtpl/...")
        cross_results = []
        cross_check_sites_rtpl(sites_path, rtpl_path, cross_results, base_path)
        if cross_results:
            print_results(cross_results, args.verbose)
            all_results.extend(cross_results)
        else:
            print("  ✓ All sites have matching rtpl/settings/chans files")
        print()

    
    print("=" * 60)
    print("Summary:")
    
    critical = sum(1 for r in all_results if r.severity == Severity.CRITICAL)
    errors = sum(1 for r in all_results if r.severity == Severity.ERROR)
    warnings = sum(1 for r in all_results if r.severity == Severity.WARNING)
    infos = sum(1 for r in all_results if r.severity == Severity.INFO)
    
    if critical > 0:
        print(f"  ❌ {critical} Critical")
    if errors > 0:
        print(f"  ❌ {errors} Errors")
    if warnings > 0:
        print(f"  ⚠️  {warnings} Warnings")
    if infos > 0:
        print(f"  ℹ️  {infos} Notes")
        
    if not all_results:
        print("  ✅ All checks passed!")
        sys.exit(0)
    elif critical > 0 or errors > 0:
        print(f"\n  ⚠️  Please fix errors before starting slftp!")
        sys.exit(1)
    else:
        print(f"\n  ℹ️  Only notes/warnings - slftp should start")
        sys.exit(0)

if __name__ == '__main__':
    main()
