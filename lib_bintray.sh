# Shell library to handle uploads & publishing of artifacts to Bintray

# All functions get their settings from global variables:
#
# * Authentication:
#   * BINTRAY_USER
#   * BINTRAY_API_KEY - to be passed as secret env variable#
# * Package ID / upload coordinates:
#   * BINTRAY_ACCOUNT
#   * BINTRAY_REPO
#   * BINTRAY_PACKAGE
#   * BINTRAY_VERSION
# * Debian-specific settings:
#   * BINTRAY_DEB_DISTRIBUTION
#   * BINTRAY_DEB_ARCH
#   * BINTRAY_DEB_COMPONENT
# * Debug options:
#   * BINTRAY_CURL_ARGS - set to `-vv` for verbose output

##
# Creates version for a package at Bintray
bintray_create_version()
{
	echo "bintray_create_version(repo=${BINTRAY_REPO}, package=${BINTRAY_PACKAGE}, version=${BINTRAY_VERSION})"

	curl $BINTRAY_CURL_ARGS -f \
		"-u$BINTRAY_USER:$BINTRAY_API_KEY" \
		-H "Content-Type: application/json" \
		-X POST "https://api.bintray.com/packages/${BINTRAY_ACCOUNT}/${BINTRAY_REPO}/${BINTRAY_PACKAGE}/versions" \
		--data "{ \"name\": \"$BINTRAY_VERSION\", \"release_notes\": \"auto\", \"released\": \"\" }"
#         --data "{ \"name\": \"$version\", \"release_notes\": \"auto\", \"release_url\": \"$BASE_DESC/$RPM_NAME\", \"released\": \"\" }"
}

##
# Uploads deb package to Bintray.
#
# Input:
# $1 = filename to upload
bintray_upload_deb()
{
	local filename="$1"

	echo "bintray_upload_deb(repo=${BINTRAY_REPO}, package=${BINTRAY_PACKAGE}, version=${BINTRAY_VERSION}, filename=${filename})"

	curl $BINTRAY_CURL_ARGS -f \
		-T "$filename" \
		"-u$BINTRAY_USER:$BINTRAY_API_KEY" \
		-H "X-Bintray-Package: $BINTRAY_PACKAGE" \
		-H "X-Bintray-Version: $BINTRAY_VERSION" \
		-H "X-Bintray-Debian-Distribution: $BINTRAY_DEB_DISTRIBUTION" \
		-H "X-Bintray-Debian-Architecture: $BINTRAY_DEB_ARCH" \
		-H "X-Bintray-Debian-Component: $BINTRAY_DEB_COMPONENT" \
		https://api.bintray.com/content/$BINTRAY_ACCOUNT/$BINTRAY_REPO/
}

bintray_publish_version()
{
	echo "bintray_publish_version(repo=${BINTRAY_REPO}, package=${BINTRAY_PACKAGE}, version=${BINTRAY_VERSION})"

	curl $BINTRAY_CURL_ARGS -f \
		"-u$BINTRAY_USER:$BINTRAY_API_KEY" \
		-H "Content-Type: application/json" \
		-X POST "https://api.bintray.com/content/$BINTRAY_ACCOUNT/$BINTRAY_REPO/$BINTRAY_PACKAGE/$BINTRAY_VERSION/publish" \
		--data "{ \"discard\": \"false\" }"
}
