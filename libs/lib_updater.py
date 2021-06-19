#!/usr/bin/python3
"""Script for updating used libraries or checking if new versions are available"""

import os
import urllib.request
import zipfile
import json
import shutil
import re
import fnmatch
import glob

EXTRACT_DIR = "_TEMP_"


def dl_and_unzip(url, exp_dirname) -> str:
    """Download file from given url and unzip it
    Args
        url: link to a ZIP file to download
        exp_dirname: expected dirname of the extracted content (used to match against)
    Returns
        foldername were the content was extracted to
    """
    if url[-3:] != "zip":
        raise Exception("Only zip files are supported")
    zip_path, _ = urllib.request.urlretrieve(url)
    with zipfile.ZipFile(zip_path, "r") as f:
        f.extractall(EXTRACT_DIR)

    foldername = ""
    for dirnames in os.listdir(EXTRACT_DIR):
        if exp_dirname.lower() in dirnames.lower():
            foldername = dirnames
            break

    if foldername == "":
        raise Exception("No matching folder found")

    return foldername


def get_HEAD_github_commit_info(owner, repository_name, branchname="master") -> str:
    """Gets the latest commit sha and message for the given repository
    Args
        owner: owner name of the project
        repository_name: name of repository
    Returns
        sha and message of commit
    """
    content = urllib.request.urlopen(
        "https://api.github.com/repos/" + owner + "/" + repository_name + "/branches/" + branchname)
    j = json.load(content)
    sha = j["commit"]["sha"]
    msg = j["commit"]["commit"]["message"]
    return sha, msg


def write_versioninfo(path, sha, msg) -> None:
    """Write versioninfo into _versioninfo_.txt file - overwrites existing file
    Args
        path: path to directory
        sha: sha hash
        msg: commit message
    """
    f = open(os.path.join(path, "_versioninfo_.txt"), "w")
    f.write(sha)
    f.write("\n")
    f.writelines(msg)
    f.close()


def cleanup_tempdir() -> None:
    """Delete temporary directory with all its content
    Args
        path: path to temp directory
    """
    shutil.rmtree(EXTRACT_DIR)


def update_mORMot():
    """Update mORMot files from Github repository"""
    LIB_DST_FOLDERNAME = "mORMot"  # lib dir name in slftp
    print("Updating {}".format(LIB_DST_FOLDERNAME))
    url = "https://github.com/synopse/mORMot/archive/master.zip"
    foldername = dl_and_unzip(url, LIB_DST_FOLDERNAME)
    author, repo_name = "synopse", "mORMot"
    sha_hash, commit_msg = get_HEAD_github_commit_info(author, repo_name)
    # commit_url = "https://github.com/" + author + \
    #    "/" + repo_name + "/commit/" + sha_hash
    # delete unwanted files
    mainpath = os.path.join(EXTRACT_DIR, foldername)
    shutil.rmtree(os.path.join(mainpath, ".github"))
    shutil.rmtree(os.path.join(mainpath, "CrossPlatform", "templates"))
    shutil.rmtree(os.path.join(mainpath, "SQLite3", "DDD"))
    shutil.rmtree(os.path.join(mainpath, "SQLite3", "Documentation"))
    shutil.rmtree(os.path.join(mainpath, "SQLite3", "Samples"))
    shutil.rmtree(os.path.join(mainpath, "SQLite3", "amalgamation"))
    shutil.rmtree(os.path.join(mainpath, "static", "arm-linux"))
    shutil.rmtree(os.path.join(mainpath, "static", "i386-darwin"))
    shutil.rmtree(os.path.join(mainpath, "static", "i386-freebsd"))
    for dirpath in glob.glob(os.path.join(mainpath, "static", "*-android")):
        shutil.rmtree(dirpath)
    shutil.rmtree(os.path.join(mainpath, "SyNode"))
    shutil.rmtree(os.path.join(mainpath, "Packages"))
    shutil.rmtree(os.path.join(mainpath, "SynDBDataset"))
    shutil.rmtree(os.path.join(mainpath, "RTL7"))
    # delete unwanted file types
    filetypes = ["*.tmpl", "*.dpk", "*.bdsproj", "*.proj", "*.rc", "*.dproj", "*.ico",
                 "*.resources", "*.res", "*.bmp", "*.cfg*", "*.json*", "*.lpi*", "*.dpr*", "*.png", "*.bat",
                 "*.c", "*.manifest*", "build-fpc*", "c-fpc*", ".git*"]
    for filetype_glob in filetypes:
        result = []
        for root, _, files in os.walk(mainpath, topdown=True):
            # case insensitive
            result += [os.path.join(root, j) for j in files if re.match(
                fnmatch.translate(filetype_glob), j, re.IGNORECASE)]
        for filepath in result:
            os.remove(filepath)
    # copy Synopse.inc into SQLite3 dir
    shutil.copy(os.path.join(mainpath, "Synopse.inc"),
                os.path.join(mainpath, "SQLite3", "Synopse.inc"))
    # write commit info file
    write_versioninfo(mainpath, sha_hash, commit_msg)
    # remove existing directory in libs folder and copy new dir over
    shutil.rmtree(LIB_DST_FOLDERNAME)
    shutil.copytree(mainpath, LIB_DST_FOLDERNAME)
    print("Update succeeded")
    print("YOU ALSO NEED TO UPDATE ZeosLib NOW OR MAKE SURE THE Zeos INC FILES ARE KEPT!!!")
    print("YOU ALSO NEED TO UPDATE ZeosLib NOW OR MAKE SURE THE Zeos INC FILES ARE KEPT!!!")
    print("YOU ALSO NEED TO UPDATE ZeosLib NOW OR MAKE SURE THE Zeos INC FILES ARE KEPT!!!")


def update_FastMM5():
    """Update FastMM5 files from Github repository"""
    LIB_DST_FOLDERNAME = "FastMM5"  # lib dir name in slftp
    print("Updating {}".format(LIB_DST_FOLDERNAME))
    url = "https://github.com/pleriche/FastMM5/archive/master.zip"
    foldername = dl_and_unzip(url, LIB_DST_FOLDERNAME)
    author, repo_name = "pleriche", "FastMM5"
    sha_hash, commit_msg = get_HEAD_github_commit_info(author, repo_name)
    # commit_url = "https://github.com/" + author + \
    #    "/" + repo_name + "/commit/" + sha_hash
    mainpath = os.path.join(EXTRACT_DIR, foldername)
    # clear existing directory in libs folder
    shutil.rmtree(LIB_DST_FOLDERNAME)
    os.mkdir(LIB_DST_FOLDERNAME)
    # copy needed files
    shutil.copy(os.path.join(mainpath, "FastMM5.pas"),
                os.path.join(LIB_DST_FOLDERNAME, "FastMM5.pas"))
    shutil.copy(os.path.join(mainpath, "README.md"),
                os.path.join(LIB_DST_FOLDERNAME, "README.md"))
    # write commit info file
    write_versioninfo(LIB_DST_FOLDERNAME, sha_hash, commit_msg)
    print("Update succeeded")


def update_rcmdline():
    """Update TCommandLineReader files from Github repository"""
    LIB_DST_FOLDERNAME = "rcmdline"  # lib dir name in slftp
    print("Updating {}".format(LIB_DST_FOLDERNAME))
    url = "https://github.com/benibela/rcmdline/archive/master.zip"
    foldername = dl_and_unzip(url, LIB_DST_FOLDERNAME)
    author, repo_name = "benibela", "rcmdline"
    sha_hash, commit_msg = get_HEAD_github_commit_info(author, repo_name)
    # commit_url = "https://github.com/" + author + \
    #    "/" + repo_name + "/commit/" + sha_hash
    mainpath = os.path.join(EXTRACT_DIR, foldername)
    # clear existing directory in libs folder
    shutil.rmtree(LIB_DST_FOLDERNAME)
    os.mkdir(LIB_DST_FOLDERNAME)
    # copy needed files
    shutil.copy(os.path.join(mainpath, "rcmdline.pas"),
                os.path.join(LIB_DST_FOLDERNAME, "rcmdline.pas"))
    # write commit info file
    write_versioninfo(LIB_DST_FOLDERNAME, sha_hash, commit_msg)
    print("Update succeeded")


def update_TRegExpr():
    """Update TRegExpr files from Github repository"""
    LIB_DST_FOLDERNAME = "TRegExpr"  # lib dir name in slftp
    print("Updating {}".format(LIB_DST_FOLDERNAME))
    url = "https://github.com/andgineer/TRegExpr/archive/master.zip"
    foldername = dl_and_unzip(url, LIB_DST_FOLDERNAME)
    author, repo_name = "andgineer", "TRegExpr"
    sha_hash, commit_msg = get_HEAD_github_commit_info(author, repo_name)
    # commit_url = "https://github.com/" + author + \
    #    "/" + repo_name + "/commit/" + sha_hash
    mainpath = os.path.join(EXTRACT_DIR, foldername, "src")
    # rewrite unit name to avoid conflicts with FPC regexpr.pas
    with open(os.path.join(mainpath, "regexpr.pas"), "r") as sources:
        lines = sources.readlines()
    with open(os.path.join(mainpath, "regexpr.pas"), "w") as sources:
        for line in lines:
            match = re.search(r"unit\s(regexpr)\;", line)
            if match:
                sources.write(line.replace(match.group(1), "RegExpr"))
            else:
                sources.write(line)
    # clear existing directory in libs folder
    shutil.rmtree(LIB_DST_FOLDERNAME)
    os.mkdir(LIB_DST_FOLDERNAME)
    # copy needed files
    shutil.copy(os.path.join(mainpath, "regexpr.pas"),
                os.path.join(LIB_DST_FOLDERNAME, "RegExpr.pas"))
    shutil.copy(os.path.join(mainpath, "regexpr_compilers.inc"),
                os.path.join(LIB_DST_FOLDERNAME, "regexpr_compilers.inc"))
    shutil.copy(os.path.join(mainpath, "regexpr_unicodedata.pas"),
                os.path.join(LIB_DST_FOLDERNAME, "regexpr_unicodedata.pas"))
    # write commit info file
    write_versioninfo(LIB_DST_FOLDERNAME, sha_hash, commit_msg)
    print("Update succeeded")


def update_PasMP():
    """Update PasMP files from Github repository"""
    LIB_DST_FOLDERNAME = "pasmp"  # lib dir name in slftp
    print("Updating {}".format(LIB_DST_FOLDERNAME))
    url = "https://github.com/BeRo1985/pasmp/archive/master.zip"
    foldername = dl_and_unzip(url, LIB_DST_FOLDERNAME)
    author, repo_name = "BeRo1985", "pasmp"
    sha_hash, commit_msg = get_HEAD_github_commit_info(author, repo_name)
    # commit_url = "https://github.com/" + author + \
    #    "/" + repo_name + "/commit/" + sha_hash
    mainpath = os.path.join(EXTRACT_DIR, foldername)
    # clear existing directory in libs folder
    shutil.rmtree(LIB_DST_FOLDERNAME)
    os.mkdir(LIB_DST_FOLDERNAME)
    # copy needed files
    shutil.copy(os.path.join(mainpath, "src", "PasMP.pas"),
                os.path.join(LIB_DST_FOLDERNAME, "PasMP.pas"))
    # write commit info file
    write_versioninfo(LIB_DST_FOLDERNAME, sha_hash, commit_msg)
    print("Update succeeded")


def update_FLRE():
    """Update FLRE files from Github repository"""
    LIB_DST_FOLDERNAME = "FLRE"  # lib dir name in slftp
    print("Updating {}".format(LIB_DST_FOLDERNAME))
    url = "https://github.com/BeRo1985/flre/archive/master.zip"
    foldername = dl_and_unzip(url, LIB_DST_FOLDERNAME)
    author, repo_name = "BeRo1985", "flre"
    sha_hash, commit_msg = get_HEAD_github_commit_info(author, repo_name)
    # commit_url = "https://github.com/" + author + \
    #    "/" + repo_name + "/commit/" + sha_hash
    mainpath = os.path.join(EXTRACT_DIR, foldername)
    # clear existing directory in libs folder
    shutil.rmtree(LIB_DST_FOLDERNAME)
    os.mkdir(LIB_DST_FOLDERNAME)
    # copy needed files
    shutil.copy(os.path.join(mainpath, "src", "FLRE.pas"),
                os.path.join(LIB_DST_FOLDERNAME, "FLRE.pas"))
    shutil.copy(os.path.join(mainpath, "src", "PUCU.pas"),
                os.path.join(LIB_DST_FOLDERNAME, "PUCU.pas"))
    # write commit info file
    write_versioninfo(LIB_DST_FOLDERNAME, sha_hash, commit_msg)
    print("Update succeeded")


def update_BeRoHighResolutionTimer():
    """Update BeRoHighResolutionTimer files from Github repository"""
    LIB_DST_FOLDERNAME = "BeRoHighResolutionTimer"  # lib dir name in slftp
    print("Updating {}".format(LIB_DST_FOLDERNAME))
    url = "https://github.com/BeRo1985/flre/archive/master.zip"
    foldername = dl_and_unzip(url, 'flre')  # it's part of FLRE repository
    author, repo_name = "BeRo1985", "flre"
    sha_hash, commit_msg = get_HEAD_github_commit_info(author, repo_name)
    # commit_url = "https://github.com/" + author + \
    #    "/" + repo_name + "/commit/" + sha_hash
    mainpath = os.path.join(EXTRACT_DIR, foldername)
    # clear existing directory in libs folder
    shutil.rmtree(LIB_DST_FOLDERNAME)
    os.mkdir(LIB_DST_FOLDERNAME)
    # copy needed files
    shutil.copy(os.path.join(mainpath, "examples", "common", "BeRoHighResolutionTimer.pas"),
                os.path.join(LIB_DST_FOLDERNAME, "BeRoHighResolutionTimer.pas"))
    # write commit info file
    write_versioninfo(LIB_DST_FOLDERNAME, sha_hash, commit_msg)
    print("Update succeeded")


def update_ZeosLib():
    """Update ZeosLib files from Github mirror repository"""
    LIB_DST_FOLDERNAME = "ZeosLib"  # lib dir name in slftp
    print("Updating {}".format(LIB_DST_FOLDERNAME))
    url = "https://github.com/marsupilami79/zeoslib/archive/8.0-patches.zip"
    foldername = dl_and_unzip(url, LIB_DST_FOLDERNAME)
    author, repo_name, branchname = "marsupilami79", "zeoslib", "8.0-patches"
    sha_hash, commit_msg = get_HEAD_github_commit_info(
        author, repo_name, branchname)
    # commit_url = "https://github.com/" + author + \
    #    "/" + repo_name + "/commit/" + sha_hash
    # delete unwanted files
    mainpath = os.path.join(EXTRACT_DIR, foldername, "src")
    shutil.rmtree(os.path.join(mainpath, "component"))
    shutil.rmtree(os.path.join(mainpath, "webservice"))
    # delete unwanted file types
    filetypes = ["repl.*", "ZDbcAdo*", "ZDbcASA*", "ZDbcDbLib*", "ZDbcInterbase6*", "ZDbcODBC*",
                 "ZDbcOleDB*", "ZDbcOracle*", "ZDbcPostgreSql*", "ZDbcSqLite*", "ZOleDB*", "ZPlainAdo*",
                 "ZPlainASA*", "ZPlainDbLib*", "ZPlainFirebird*", "ZPlainODBC*", "ZPlainOleDB*", "ZPlainOracle*",
                 "ZPlainPostgreSql*", "ZPlainSqLite*"]
    for filetype_glob in filetypes:
        result = []
        for root, _, files in os.walk(mainpath, topdown=True):
            # case insensitive
            result += [os.path.join(root, j) for j in files if re.match(
                fnmatch.translate(filetype_glob), j, re.IGNORECASE)]
        for filepath in result:
            os.remove(filepath)
    # set defines to disable stuff which is useless for us
    with open(os.path.join(mainpath, "Zeos.inc"), "r") as sources:
        lines = sources.readlines()
    with open(os.path.join(mainpath, "Zeos.inc"), "w") as sources:
        for line in lines:
            match = re.match(r"\{\.\$DEFINE\s+ZEOS\_DISABLE\_(.*?)\}", line)
            if match:
                if match.group(1).lower() == 'MYSQL'.lower():
                    sources.write(line)
                else:
                    sources.write(
                        '{$DEFINE ZEOS_DISABLE_' + match.group(1) + '}\n')
            else:
                sources.write(line)
    # write commit info file
    write_versioninfo(mainpath, sha_hash, commit_msg + "\n\n\n" +
                      "## Original repository is located at https://sourceforge.net/projects/zeoslib/ ##")
    # remove existing directory in libs folder and copy new dir over
    shutil.rmtree(LIB_DST_FOLDERNAME)
    shutil.copytree(mainpath, LIB_DST_FOLDERNAME)
    # copy final Zeos includes to mORMot directory
    shutil.copy(os.path.join(LIB_DST_FOLDERNAME, "Zeos.inc"),
                os.path.join("mORMot", "Zeos.inc"))
    shutil.copy(os.path.join(LIB_DST_FOLDERNAME, "ZeosLazarus.inc"),
                os.path.join("mORMot", "ZeosLazarus.inc"))
    print("Update succeeded")


def check_lkJSON_version():
    """Check if a new version of lkJSON is available

    Note: Update is not automated"""
    # LIB_DST_FOLDERNAME = "lkJSON" # lib dir name in slftp
    LIB_NAME = "LkJSON"
    print("Checking for new version of {}".format(LIB_NAME))
    CURRENT_VERSION = "1.07"
    url = "https://sourceforge.net/projects/lkjson/best_release.json"
    content = urllib.request.urlopen(url)
    j = json.load(content)
    api_filename = j["release"]["filename"]
    name, version = api_filename.split("-")
    if LIB_NAME.lower() in name.lower() and CURRENT_VERSION.lower() in version.lower():
        print("Version is up2date!")
    else:
        print("Newer version available.")
    print("Checking succeeded")


def check_LibTar_version():
    """Check if a new version of LibTar is available

    Note: Update is not automated"""
    # LIB_DST_FOLDERNAME = "LibTar" # lib dir name in slftp
    LIB_NAME = "LibTar"
    print("Checking for new version of {}".format(LIB_NAME))
    CURRENT_VERSION = "2.1.2"
    url = "http://www.destructor.de/libtar/index.htm"
    content = urllib.request.urlopen(url).read().decode('utf-8')
    match = re.search(r'<h1>(.*?)<\/h1>', content)
    name = match.group(1)
    match = re.search(r'<td>(\d.\d.\d), \d{4}\-\d{2}\-\d{2}<\/td>', content)
    version = match.group(1)
    if LIB_NAME.lower() == name.lower() and CURRENT_VERSION.lower() == version.lower():
        print("Version is up2date!")
    else:
        print("Newer version available.")
    print("Checking succeeded")


def update_Indy10():
    """Update Indy10 files from Github repository"""
    LIB_DST_FOLDERNAME = "Indy10"  # lib dir name in slftp
    print("Updating {}".format(LIB_DST_FOLDERNAME))
    url = "https://github.com/IndySockets/Indy/archive/master.zip"
    name_in_archive = LIB_DST_FOLDERNAME[:-2]
    foldername = dl_and_unzip(url, name_in_archive)
    author, repo_name = "IndySockets", "Indy"
    sha_hash, commit_msg = get_HEAD_github_commit_info(author, repo_name)
    # commit_url = "https://github.com/" + author + \
    #    "/" + repo_name + "/commit/" + sha_hash
    # delete unwanted files
    mainpath = os.path.join(EXTRACT_DIR, foldername, "Lib")
    shutil.rmtree(os.path.join(mainpath, "Core", "IconsDotNet"))
    shutil.rmtree(os.path.join(mainpath, "Core", "Res"))
    shutil.rmtree(os.path.join(mainpath, "Protocols", "IconsDotNet"))
    # delete unwanted file types
    filetypes = ["*.tmpl", "*.dpk", "*.bdsproj", "*.proj", "*.rc", "*.dproj", "*.ico",
                 "*.resources", "*.res", "*.bmp", "*.cfg*", "*.dpkl*", "*.psd*", "*.dcr*", "*.xpm*",
                 "*.lpk*", "*.fpc", "*.resx"]
    for filetype_glob in filetypes:
        result = []
        for root, _, files in os.walk(mainpath, topdown=True):
            # case insensitive
            result += [os.path.join(root, j) for j in files if re.match(
                fnmatch.translate(filetype_glob), j, re.IGNORECASE)]
        for filepath in result:
            os.remove(filepath)
    # remove existing directory in libs folder and copy wanted dirs over
    shutil.rmtree(LIB_DST_FOLDERNAME)
    shutil.copytree(os.path.join(mainpath, "Core"),
                    os.path.join(LIB_DST_FOLDERNAME, "Core"))
    shutil.copytree(os.path.join(mainpath, "Protocols"),
                    os.path.join(LIB_DST_FOLDERNAME, "Protocols"))
    shutil.copytree(os.path.join(mainpath, "System"),
                    os.path.join(LIB_DST_FOLDERNAME, "System"))
    # write commit info file
    write_versioninfo(LIB_DST_FOLDERNAME, sha_hash, commit_msg)
    print("Update succeeded")


current_dir = os.getcwd()
if not current_dir.endswith("libs"):
    print("Script can only be executed from libs folder!")
    exit()

print("Select one of the following numbers to update the library:")
print("\t" + " 1 - BeRoHighResolutionTimer")
print("\t" + " 2 - FastMM5")
print("\t" + " 3 - FLRE")
print("\t" + " 4 - Indy10")
print("\t" + " 5 - LibTar (version check only)")
print("\t" + " 6 - lkJSON (version check only)")
print("\t" + " 7 - mORMot")
print("\t" + " 8 - pasmp")
print("\t" + " 9 - rcmdline")
print("\t" + "10 - TRegExpr")
print("\t" + "11 - ZeosLib")
val = input("Enter a number: ")

val = int(val)
if val == 1:
    update_BeRoHighResolutionTimer()
elif val == 2:
    update_FastMM5()
elif val == 3:
    update_FLRE()
elif val == 4:
    update_Indy10()
elif val == 5:
    check_LibTar_version()
elif val == 6:
    check_lkJSON_version()
elif val == 7:
    update_mORMot()
elif val == 8:
    update_PasMP()
elif val == 9:
    update_rcmdline()
elif val == 10:
    update_TRegExpr()
elif val == 11:
    update_ZeosLib()
else:
    print("Invalid number!")
    exit()

# final cleanup
if os.path.isdir(EXTRACT_DIR):
    cleanup_tempdir()
