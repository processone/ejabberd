#!/bin/sh

# Build installers for Linux/x64 and Linux/arm64.
#
# Author: Holger Weiss <holger@zedat.fu-berlin.de>.
#
# Copyright (c) 2022 ProcessOne, SARL.
# All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

set -e
set -u

myself=${0##*/}
architectures='x64 arm64'
iteration=1

usage()
{
	echo >&2 "Usage: $myself [-i <iteration>]"
	exit 2
}

while getopts i: opt
do
	case $opt in
	i)
		iteration="$OPTARG"
		;;
	\?)
		usage
		;;
	esac
done
shift $((OPTIND - 1))

if ! [ -e 'mix.exs' ] || ! [ -e "tools/$myself" ]
then
	echo >&2 "Please call this script from the repository's root directory."
	exit 2
elif [ $# -ne 0 ]
then
	usage
fi
if type 'makeself' >'/dev/null'
then makeself='makeself'
elif type 'makeself.sh' >'/dev/null'
then makeself='makeself.sh'
else
	echo >&2 'This script requires makeself: https://makeself.io'
	exit 1
fi

rel_name='ejabberd'
rel_vsn=$(git describe --tags | sed -e 's/-g.*//' -e 's/-/./' | tr -d '[:space:]')
home_url='https://www.ejabberd.im'
doc_url='https://docs.ejabberd.im'
upgrade_url="$doc_url/admin/upgrade/#specific-version-upgrade-notes"
admin_url="$doc_url/admin/install/next-steps/#administration-account"
default_code_dir="/opt/$rel_name-$rel_vsn"
default_data_dir="/opt/$rel_name"
tmp_dir=$(mktemp -d "/tmp/.$rel_name.XXXXXX")

trap 'rm -rf "$tmp_dir"' INT TERM EXIT
umask 022

create_help_file()
{
	local file="$1"

	cat >"$file" <<-EOF
	This is the $rel_name $rel_vsn-$iteration installer for linux-gnu-$arch
	
	Visit:
	  $home_url
	
	ejabberd documentation site:
	  $doc_url
	
	EOF
}

create_setup_script()
{
	local dir="$1"

	cat >"$dir/setup" <<-EOF
	#!/bin/sh

	set -e
	set -u

	export PATH='/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin'

	user_agrees()
	{
		local question="\$*"

		if [ -t 0 ]
		then
			read -p "\$question (y/n) [n] " response
			case "\$response" in
			[Yy]|[Yy][Ee][Ss])
				return 0
				;;
			[Nn]|[Nn][Oo]|'')
				return 1
				;;
			*)
				echo 'Please respond with "yes" or "no".'
				user_agrees "\$question"
				;;
			esac
		else # Assume 'yes' if not running interactively.
			return 0
		fi
	}

	if [ \$(id -u) = 0 ]
	then
		is_superuser=true
	else
		is_superuser=false
		echo "Running without superuser privileges (installer wasn't invoked"
		echo 'with "sudo"), cannot perform system-wide installation this way.'
		if ! user_agrees 'Continue anyway?'
		then
			echo 'Aborting installation.'
			exit 1
		fi
	fi

	if [ \$is_superuser = true ]
	then
		code_dir='$default_code_dir'
		data_dir='$default_data_dir'
		user_name='$rel_name'
		group_name='$rel_name'
	elif user_agrees "Install $rel_name below \$HOME/opt?"
	then
		code_dir="\$HOME/opt/$rel_name-$rel_vsn"
		data_dir="\$HOME/opt/$rel_name"
		user_name="\$(id -u -n)"
		group_name="\$(id -g -n)"
	else
		read -p 'Installation prefix: ' prefix
		if printf '%s' "\$prefix" | grep -q '^/'
		then
			code_dir="\$prefix/$rel_name-$rel_vsn"
			data_dir="\$prefix/$rel_name"
			user_name="\$(id -u -n)"
			group_name="\$(id -g -n)"
		else
			echo >&2 'Prefix must be specified as an absolute path.'
			echo >&2 'Aborting installation.'
			exit 1
		fi
	fi

	prefix="\$(dirname "\$code_dir")"
	conf_dir="\$data_dir/conf"
	pem_file="\$conf_dir/server.pem"
	uninstall_file="\$code_dir/uninstall.txt"

	if [ -e '/run/systemd/system' ]
	then is_systemd=true
	else is_systemd=false
	fi
	if [ -e "\$data_dir" ]
	then is_upgrade=true
	else is_upgrade=false
	fi
	if id -u "\$user_name" >'/dev/null' 2>&1
	then user_exists=true
	else user_exists=false
	fi

	echo
	echo 'The following installation paths will be used:'
	echo "- \$code_dir"
	if [ \$is_upgrade = true ]
	then echo "- \$data_dir (existing files won't be modified)"
	else echo "- \$data_dir (for configuration, database, and log files)"
	fi
	if [ \$is_superuser = true ]
	then
		if [ \$is_systemd = true ]
		then
			echo '- /etc/systemd/system/$rel_name.service'
			if [ \$is_upgrade = false ]
			then echo 'The $rel_name service is going to be enabled and started.'
			fi
		fi
		if [ \$user_exists = false ]
		then echo 'The $rel_name user is going to be created.'
		fi
	fi
	if ! user_agrees 'Install $rel_name $rel_vsn now?'
	then
		echo 'Aborting installation.'
		exit 1
	fi
	echo

	if [ \$user_exists = false ] && [ \$is_superuser = true ]
	then useradd -r -d "\$data_dir" "\$user_name"
	fi

	host=\$(hostname --fqdn 2>'/dev/null' || :)
	if [ -z "\$host" ]
	then host='localhost'
	fi

	mkdir -p "\$prefix"
	tar -cf - '$rel_name' | tar --skip-old-files -C "\$prefix" -xf -
	tar -cf - '$rel_name-$rel_vsn' | tar -C "\$prefix" -xf -

	if [ \$is_superuser = true ]
	then
		if [ \$is_upgrade = false ]
		then chown -R -h "\$user_name:\$group_name" "\$data_dir"
		fi
		chown -R -h "\$(id -u -n):\$group_name" "\$code_dir"
		chmod -R g+rX "\$code_dir"
		chmod '4750' "\$code_dir/lib/epam-"*'/priv/bin/epam'
	else
		sed -i "s/^INSTALLUSER=.*/INSTALLUSER=\"\$user_name\"/" \
		    "\$code_dir/bin/${rel_name}ctl"
		sed -i "s/^USER=.*/USER=\$user_name/" \
		    "\$code_dir/bin/$rel_name.init"
		sed -i \
		    -e "s/^User=.*/User=\$user_name/" \
		    -e "s/^Group=.*/Group=\$group_name/" \
		    "\$code_dir/bin/$rel_name.service"
	fi
	if [ "\$code_dir" != '$default_code_dir' ]
	then
		sed -i "s|$default_code_dir|\$code_dir|g" \
		    "\$code_dir/bin/$rel_name.init" \
		    "\$code_dir/bin/$rel_name.service"
	fi
	if [ "\$data_dir" != '$default_data_dir' ]
	then
		sed -i "s|$default_data_dir|\$data_dir|g" \
		    "\$code_dir/bin/${rel_name}ctl" \
		    "\$data_dir/conf/$rel_name.yml" \
		    "\$data_dir/conf/${rel_name}ctl.cfg"
	fi

	if [ \$is_upgrade = false ]
	then
		sed -i "s/ - localhost$/ - \$host/" "\$conf_dir/$rel_name.yml"
		openssl req -x509 \
		            -batch \
		            -nodes \
		            -newkey rsa:4096 \
		            -keyout "\$pem_file" \
		            -out "\$pem_file" \
		            -days 3650 \
		            -subj "/CN=\$host" >'/dev/null' 2>&1 || :
		if ! [ -e "\$pem_file" ]
		then
			echo 'Failed to create a TLS certificate for $rel_name.' >&2
		elif [ \$is_superuser = true ]
		then
			chown "\$user_name:\$group_name" "\$pem_file"
		fi
	fi

	case \$is_systemd,\$is_superuser in
	true,true)
		cp "\$code_dir/bin/$rel_name.service" '/etc/systemd/system/'
		systemctl -q daemon-reload
		if [ \$is_upgrade = false ]
		then systemctl -q --now enable '$rel_name'
		fi
		;;
	true,false)
		echo 'You might want to install a systemd unit (see the'
		echo "\$code_dir/bin directory for an example)."
		;;
	false,*)
		echo 'You might want to install an init script (see the'
		echo "\$code_dir/bin directory for an example)."
		;;
	esac

	echo
	echo '$rel_name $rel_vsn has been installed successfully.'
	echo

	cat >"\$uninstall_file" <<-_EOF
	# To uninstall $rel_name, first remove the service. If you're using systemd:
	systemctl --now disable $rel_name
	rm -f /etc/systemd/system/$rel_name.service
	
	# Remove the binary files:
	rm -rf \$code_dir
	
	# If you want to remove your configuration, database and logs:
	rm -rf \$data_dir
	_EOF
	if [ \$is_superuser = true ]
	then
		cat >>"\$uninstall_file" <<-_EOF
		
		# To remove the user running $rel_name:
		userdel \$user_name
		_EOF
	fi

	if [ \$is_upgrade = false ]
	then
		if [ \$is_systemd = true ] && [ \$is_superuser = true ]
		then
			echo 'Now you can check $rel_name is running correctly:'
			echo '  systemctl status $rel_name'
			echo
		fi
		echo 'Next you may want to edit $rel_name.yml to set up hosts,'
		echo 'register an account and grant it admin rigts, see:'
		echo
		echo '$admin_url'
	else
		echo 'Please check the following web site for upgrade notes:'
		echo
		echo '$upgrade_url'
		echo
		if [ \$is_systemd = true ] && [ \$is_superuser = true ]
		then
			echo 'If everything looks fine, restart the $rel_name service:'
			echo '  systemctl restart $rel_name'
		else
			echo 'If everything looks fine, restart the $rel_name service.'
		fi
	fi
	EOF
	chmod +x "$dir/setup"
}

for arch in $architectures
do
	tar_name="$rel_name-$rel_vsn-linux-gnu-$arch.tar.gz"
	installer_name="$rel_name-$rel_vsn-$iteration-linux-$arch.run"

	test -e "$tar_name" || tools/make-binaries
	echo "$myself: Putting together installer for $arch ..."
	tar -C "$tmp_dir" -xzpf "$tar_name"
	create_help_file "$tmp_dir/help.txt"
	create_setup_script "$tmp_dir"
	"$makeself" --help-header "$tmp_dir/help.txt" \
	    "$tmp_dir" "$installer_name" "$rel_name $rel_vsn" './setup'
	find "$tmp_dir" -mindepth 1 -delete
done
echo "$myself: Created installers successfully."
