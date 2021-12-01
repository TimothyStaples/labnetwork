message="auto-commit from on $(date)"
GIT=`which git`
REPO_DIR=~Dropbox/Tim/Post-doc/Presentations/'School Lab Speed Talk'
cd ${REPO_DIR}
${GIT} add --all .
${GIT} commit -m "$message"
${GIT} push git@github.com:TimothyStaples/labnetwork

echo "$gitPush"
