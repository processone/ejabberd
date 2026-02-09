(function () {
	// If QR lib loaded ok, show QR button on desktop devices
	if(window.QRCode) {
		const qrcode_opts = {
			text : document.location.href,
			addQuietZone: true
		};
		new QRCode(document.getElementById("qr-invite-page"), qrcode_opts);
		document.getElementById('qr-button-container').classList.add("d-md-block");
	}

	// Detect current platform and show/hide appropriate clients
	if(window.platform) {
		let platform_friendly = null;
		let platform_classname = null;
		switch(platform.os.family) {
		case "Ubuntu":
		case "Linux":
		case "Fedora":
		case "Red Hat":
		case "SuSE":
			platform_friendly = platform.os.family + " (Linux)";
			platform_classname = "linux";
			break;
		case "Linux aarch64":
			platform_friendly = "Linux mobile";
			platform_classname = "linux";
			break;
		case "Haiku R1":
			platform_friendly = "Haiku";
			platform_classname = "haiku";
			break;
		case "Windows Phone":
			platform_friendly = "Windows Phone";
			platform_classname = "windows-phone";
			break;
		case "OS X":
			if (navigator.maxTouchPoints > 1) {
				// looks like iPad to me!
				platform_friendly = "iPadOS";
				platform_classname = "ipados";
			} else {
				platform_friendly = "macOS";
				platform_classname = "macos";
			}
			break;
		default:
			if(platform.os.family.startsWith("Windows")) {
				platform_friendly = "Windows";
				platform_classname = "windows";
			} else {
				platform_friendly = platform.os.family;
				platform_classname = platform_friendly.toLowerCase();
			}
		}

		if(platform_friendly && platform_classname) {
			if(document.querySelectorAll('.client-card .client-platform-badge-'+platform_classname).length == 0) {
				// No clients recognised for this platform, do nothing
				return;
			}
			// Hide clients not for this platform
			const client_cards = document.getElementsByClassName('client-card');
			for (let card of client_cards) {
				if (card.classList.contains('app-platform-'+platform_classname))
					card.classList.add('supported-platform');
				else if (!card.classList.contains('app-platform-web'))
					card.hidden = true;
				const badges = card.querySelectorAll('.client-platform-badge');
				for (let badge of badges) {
					if (badge.classList.contains('client-platform-badge-'+platform_classname)) {
						badge.classList.add("badge-success");
						badge.classList.remove("badge-info");
					} else {
						badge.classList.add("badge-secondary");
						badge.classList.remove("badge-info");
					}
				}
			}
			const show_all_clients_button_container = document.getElementById('show-all-clients-button-container');
			show_all_clients_button_container.querySelector('.platform-name').innerHTML = platform_friendly;
			show_all_clients_button_container.classList.remove("d-none");
			document.getElementById('show-all-clients-button').addEventListener('click', function (e) {
				for (let card of client_cards)
					card.hidden = false;
				show_all_clients_button_container.hidden = true;
				e.preventDefault();
			});
		}
	}
})();
