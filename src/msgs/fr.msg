% $Id$
% Language: French (française)
% Author: Christophe Romain
% Author: Mickaël Rémond
% Author: Vincent Ricard

% jlib.hrl
{"No resource provided", "Aucune ressource fournie"}.

% mod_configure.erl
{"Choose storage type of tables", "Choisissez un type de stockage pour les tables"}.
{"RAM copy", "Copie en mémoire vive (RAM)"}.
{"RAM and disc copy", "Copie en mémoire vive (RAM) et sur disque"}.
{"Disc only copy", "Copie sur disque uniquement"}.
{"Remote copy", "Copie distante"}.
{"Stop Modules at ", "Arrêter les modules sur "}.
{"Choose modules to stop", "Sélectionnez les modules à arrêter"}.
{"Start Modules at ", "Démarrer les modules sur "}.
{"Enter list of {Module, [Options]}", "Entrez une liste de {Module, [Options]}"}.
{"List of modules to start", "Liste des modules à démarrer"}.
{"Backup to File at ", "Sauvegarde sur fichier sur "}.
{"Enter path to backup file", "Entrez le chemin vers le fichier de sauvegarde"}.
{"Path to File", "Chemin vers le fichier"}.
{"Restore Backup from File at ", "Restaurer la sauvegarde depuis le fichier sur "}.
{"Dump Backup to Text File at ", "Enregistrer la sauvegarde dans un fichier texte sur "}.
{"Enter path to text file", "Entrez le chemin vers le fichier texte"}.
{"Import User from File at ", "Importer un utilisateur depuis le fichier sur "}.
{"Enter path to jabberd1.4 spool file", "Entrez le chemin vers le fichier spool jabberd1.4"}.
{"Import Users from Dir at ", "Importer des utilisateurs depuis le répertoire sur "}.
{"Enter path to jabberd1.4 spool dir", "Entrez le chemin vers le répertoire de spool jabberd1.4"}.
{"Path to Dir", "Chemin vers le répertoire"}.
{"Access Control List Configuration", "Configuration des droits (ACL)"}.
{"Access control lists", "Droits (ACL)"}.
{"Access Configuration", "Configuration d'accès"}.
{"Access rules", "Règles d'accès"}.
{"Administration of ", "Administration de "}.
{"Action on user", "Action sur l'utilisateur"}.
{"Edit Properties", "Modifier les propriétés"}.
{"Remove User", "Supprimer l'utilisateur"}.
{"Database", "Base de données"}.
{"Outgoing s2s Connections", "Connexions s2s sortantes"}.
{"Import Users From jabberd 1.4 Spool Files", "Importer des utilisateurs depuis un fichier spool Jabberd 1.4"}.
{"Database Tables Configuration at ", "Configuration des tables de base de données sur "}.
{"Restart Service", "Redémarrer le service"}.
{"Shut Down Service", "Arrêter le service"}.
{"Delete User", "Supprimer l'utilisateur"}.
{"End User Session", "Terminer la session de l'utilisateur"}.
{"Get User Password", "Récupérer le mot de passe de l'utilisateur"}.
{"Change User Password", "Changer le mot de passe de l'utilisateur"}.
{"Get User Last Login Time", "Récupérer la dernière date de connexion de l'utilisateur"}.
{"Get User Statistics", "Récupérer les statistiques de l'utilisateur"}.
{"Get Number of Registered Users", "Récupérer le nombre d'utilisateurs enregistrés"}.
{"Get Number of Online Users", "Récupérer le nombre d'utilisateurs en ligne"}.
{"User Management", "Gestion des utilisateurs"}.
{"Time delay", "Délais"}.
{"Password Verification", "Vérification du mot de passe"}.
{"Number of registered users", "Nombre d'utilisateurs enregistrés"}.
{"Number of online users", "Nombre d'utilisateurs en ligne"}.
{"Last login", "Dernière connexion"}.
{"Roster size", "Taille de la liste de contacts"}.
{"IP addresses", "Adresses IP"}.
{"Resources", "Ressources"}.


% src/ejabberd_c2s.erl
{"Use of STARTTLS required", "L'utilisation de STARTTLS est impérative"}.
{"Replaced by new connection", "Remplacé par une nouvelle connexion"}.

% mod_disco.erl
{"Configuration", "Configuration"}.
{"Online Users", "Utilisateurs en ligne"}.
{"All Users", "Tous les utilisateurs"}.
{"To ~s", "A ~s"}.
{"From ~s", "De ~s"}.
{"Running Nodes", "Noeuds actifs"}.
{"Stopped Nodes", "Noeuds arrêtés"}.
{"Access Control Lists", "Droits (ACL)"}.
{"Access Rules", "Règles d'accès"}.
{"Modules", "Modules"}.
{"Start Modules", "Modules de démarrage"}.
{"Stop Modules", "Modules d'arrêt"}.
{"Backup Management", "Gestion des sauvegardes"}.
{"Backup", "Sauvegarde"}.
{"Restore", "Restauration"}.
{"Dump to Text File", "Sauvegarder dans un fichier texte"}.
{"Import File", "Importer un fichier"}.
{"Import Directory", "Importer une répertoire"}.

% mod_register.erl
{"Users are not allowed to register accounts so fast", "Les utilisateurs ne sont pas autorisés à enregistrer des comptes si rapidement"}.
{"Choose a username and password to register with this server", "Choisissez un nom d'utilisateur et un mot de passe pour s'enregistrer sur ce serveur"}.

% src/mod_vcard_ldap.erl
{"Erlang Jabber Server", "Serveur Jabber Erlang"}.
{"ejabberd vCard module", "Module vCard ejabberd"}.
{"Email", "Email"}.
{"Search Results for ", "Résultats de recherche pour "}.
{"Jabber ID", "Jabber ID"}.

% mod_vcard.erl
{"You need an x:data capable client to search", "Vous avez besoin d'un client supportant x:data pour faire une recherche"}.
{"Search users in ", "Rechercher des utilisateurs "}.
{"vCard User Search", "Recherche dans l'annnuaire"}.
{"Fill in fields to search for any matching Jabber User", "Remplissez les champs pour rechercher un utilisateur Jabber"}.
{"Fill in the form to search for any matching Jabber User (Add * to the end of field to match substring)", "Remplissez le formulaire pour recherche un utilisateur Jabber (Ajouter * à la fin du champ pour chercher n'importe quelle fin de chaîne"}.
{"User",              "Utilisateur"}.
{"Full Name", 	      "Nom complet"}.
{"Name", 	      "Nom"}.
{"Middle Name",       "Autre nom"}.
{"Family Name",       "Nom de famille"}.
{"Nickname", 	      "Surnom"}.
{"Birthday", 	      "Date d'anniversaire"}.
{"Country", 	      "Pays"}.
{"City", 	      "Ville"}.
{"Organization Name", "Nom de l'organisation"}.
{"Organization Unit", "Unité de l'organisation"}.

% mod_adhoc.erl
{"Commands", "Commandes"}.
{"Ping", "Ping"}.
{"Pong", "Pong"}.

% mod_announce.erl
{"Really delete message of the day?", "Confirmer la suppression du message du jour ?"}.
{"Subject", "Sujet"}.
{"Message body", "Corps du message"}.
{"No body provided for announce message", "Pas de corps de message pour l'annonce"}.
{"Announcements", "Annonces"}.
{"Send announcement to all users", "Envoyer l'annonce à tous les utilisateurs"}.
{"Send announcement to all online users", "Envoyer l'annonce à tous les utilisateurs en ligne"}.
{"Send announcement to all online users on all hosts", "Envoyer l'annonce à tous les utilisateurs en ligne sur tous les serveurs"}.
{"Set message of the day and send to online users", "Définir le message du jour et l'envoyer aux utilisateurs en ligne"}.
{"Update message of the day (don't send)", "Mise à jour du message du jour (pas d'envoi)"}.
{"Delete message of the day", "Supprimer le message du jour"}.
{"Send announcement to all users on all hosts", "Envoyer une annonce à tous les utilisateurs de tous les domaines"}.
{"Set message of the day on all hosts and send to online users", "Définir le message du jour pour tous domaines et l'envoyer aux utilisateurs en ligne"}.
{"Update message of the day on all hosts (don't send)", "Mettre à jour le message du jour sur tous les domaines (ne pas envoyer)"}.
{"Delete message of the day on all hosts", "Supprimer le message du jour sur tous les domaines"}.


% mod_pubsub/mod_pubsub.erl
{"A friendly name for the node", "Un nom convivial pour le noeud"}.
{"Choose whether to approve this entity's subscription.", "Accepter cet abonnement ?"}.
{"Roster groups allowed to subscribe", "Groupes de liste de contact autorisés à s'abonner"}.
{"Deliver payloads with event notifications", "Inclure le contenu du message avec la notification"}.
{"Notify subscribers when the node configuration changes", "Avertir les abonnés lorsque la configuration du noeud change"}.
{"Notify subscribers when the node is deleted", "Avertir les abonnés lorsque le noeud est supprimé"}.
{"Notify subscribers when items are removed from the node", "Avertir les abonnés lorsque des éléments sont supprimés sur le noeud"}.
{"Persist items to storage", "Stockage persistant des éléments"}.
{"Max # of items to persist", "Nombre maximum d'éléments à stocker"}.
{"Whether to allow subscriptions", "Autoriser l'abonnement ?"}.
{"Specify the publisher model", "Définir le modèle de publication"}.
{"Max payload size in bytes", "Taille maximum pour le contenu du message en octet"}.
{"Only deliver notifications to available users", "Envoyer les notifications uniquement aux utilisateurs disponibles"}.
{"Publish-Subscribe", "Publication-Abonnement"}.
{"ejabberd Publish-Subscribe module", "Module Publish-Subscribe d'ejabberd"}.
{"PubSub subscriber request", "Demande d'abonnement PubSub"}.
{"Node ID", "Identifiant du noeud"}.
{"Subscriber Address", "Adresse de l'abonné"}.
{"Allow this JID to subscribe to this pubsub node?", "Autoriser ce JID à s'abonner à ce noeud pubsub"}.
{"Deliver event notifications", "Envoyer les notifications d'événement"}.
{"Specify the access model", "Définir le modèle d'accès"}.
{"When to send the last published item", "A quel moment envoyer le dernier élément publié"}.

% mod_muc/mod_muc_log.erl
{"Chatroom configuration modified", "Configuration du salon modifiée"}.
{"joins the room", "rejoint le salon"}.
{"leaves the room", "quitte le salon"}.
{"has been kicked", "a été expulsé"}.
{"has been banned", "a été banni"}.
{"is now known as", "est maintenant connu comme"}.
{"Monday", "Lundi"}.
{"Tuesday", "Mardi"}.
{"Wednesday", "Mercredi"}.
{"Thursday", "Jeudi"}.
{"Friday", "Vendredi"}.
{"Saturday", "Samedi"}.
{"Sunday", "Dimanche"}.
{"January", "Janvier"}.
{"February", "Février"}.
{"March", "Mars"}.
{"April", "Avril"}.
{"May", "Mai"}.
{"June", "Juin"}.
{"July", "Juillet"}.
{"August", "Août"}.
{"September", "Septembre"}.
{"October", "Octobre"}.
{"November", "Novembre"}.
{"December", "Décembre"}.
{"Room Configuration", "Configuration du salon"}.
{"has been kicked because of an affiliation change", "a été éjecté à cause d'un changement d'autorisation"}.
{"has been kicked because the room has been changed to members-only", "a été éjecté car la salle est désormais réservée aux membres"}.
{"has been kicked because of a system shutdown", "a été éjecté en raison de l'arrêt du système"}.


% mod_muc/mod_muc.erl
{"You need an x:data capable client to register nickname", "Vous avez besoin d'un client supportant x:data pour enregistrer un pseudo"}.
{"Nickname Registration at ", "Enregistrement d'un pseudo sur "}.
{"Enter nickname you want to register", "Entrez le pseudo que vous souhaitez enregistrer"}.
{"ejabberd MUC module", "Module MUC ejabberd"}.
{"Only service administrators are allowed to send service messages", "Seuls les administrateurs du service sont autoriser à envoyer des messages de service"}.
{"Room creation is denied by service policy", "La création de salons est interdite par le service"}.
{"Conference room does not exist", "La salle de conférence n'existe pas"}.
{"Access denied by service policy", "L'accès au service est refusé"}.
{"Specified nickname is already registered", "Le pseudo demandé est déjà enregistré"}.
{"You must fill in field \"Nickname\" in the form", "Vous devez préciser le champ \"pseudo\" dans le formulaire"}.
{"Chatrooms", "Salons de discussion"}.

% mod_muc/mod_muc_room.erl
{"It is not allowed to send private messages", "L'envoi de messages privés n'est pas autorisé"}.
{"Visitors are not allowed to change their nicknames in this room", "Les visiteurs ne sont pas autorisés à changer de pseudo dans ce salon"}.
{"Allow visitors to send status text in presence updates", "Autoriser les visiteurs à envoyer un message d'état avec leur présence"}.
{"Allow visitors to change nickname", "Autoriser les visiteurs à changer de pseudo"}.
{"This participant is kicked from the room because he sent an error message", "Ce participant est expulsé du salon pour avoir envoyé un message erroné"}.
{"This participant is kicked from the room because he sent an error message to another participant", "Ce participant est expulsé du salon pour avoir envoyé un message erroné à un autre participant"}.
{"This participant is kicked from the room because he sent an error presence", "Ce participant est expulsé du salon pour avoir envoyé une présence erroné"}.
{"Make room moderated", "Rendre le salon modéré"}.
{" has set the subject to: ", " a changé le sujet pour: "}.
{"You need an x:data capable client to configure room", "Vous avez besoin d'un client supportant x:data pour configurer le salon"}.
{"Configuration for ", "Configuration pour "}.
{"Room title", "Titre du salon"}.
{"Password", "Mot de passe"}.
{"Only moderators and participants are allowed to change subject in this room", "Seuls les modérateurs et les participants peuvent changer le sujet dans ce salon"}.
{"Only moderators are allowed to change subject in this room", "Seuls les modérateurs peuvent changer le sujet dans ce salon"}.
{"Visitors are not allowed to send messages to all occupants", "Les visiteurs ne sont pas autorisés à envoyer des messages à tout les occupants"}.
{"Only occupants are allowed to send messages to the conference", "Seuls les occupants peuvent envoyer des messages à la conférence"}.
{"It is not allowed to send private messages to the conference", "Il n'est pas permis d'envoyer des messages \"normaux\" à la conférence"}.
{"Improper message type", "Mauvais type de message"}.
{"Nickname is already in use by another occupant", "Le pseudo est déjà utilisé par un autre occupant"}.
{"Nickname is registered by another person", "Le pseudo est enregistré par une autre personne"}.
{"It is not allowed to send private messages of type \"groupchat\"", "Il n'est pas permis d'envoyer des messages privés de type \"groupchat\""}.
{"Recipient is not in the conference room", "Le destinataire n'est pas dans la conférence"}.
{"Only occupants are allowed to send queries to the conference", "Seuls les occupants sont autorisés à envoyer des requêtes à la conférence"}.
{"Queries to the conference members are not allowed in this room", "Les requêtes sur les membres de la conférence ne sont pas autorisé dans ce salon"}.
{"You have been banned from this room", "Vous avez été exclus de ce salon"}.
{"Membership required to enter this room", "Vous devez être membre pour accèder à ce salon"}.
{"Password required to enter this room", "Un mot de passe est nécessaire pour accèder à ce salon"}.
{"Incorrect password", "Mot de passe incorrect"}.
{"Administrator privileges required", "Les droits d'administrateur sont nécessaires"}.
{"Moderator privileges required", "Les droits de modérateur sont nécessaires"}.
{"JID ~s is invalid", "Le JID ~s n'est pas valide"}.
{"Nickname ~s does not exist in the room", "Le pseudo ~s n'existe pas dans ce salon"}.
{"Invalid affiliation: ~s", "Affiliation invalide: ~s"}.
{"Invalid role: ~s", "Role invalide: ~s"}.
{"Owner privileges required", "Les droits de propriétaire sont nécessaires"}.
{"private, ", "privé"}.
{"This room is not anonymous", "Ce salon n'est pas anonyme"}.
{"Make room persistent", "Rendre le salon persistant"}.
{"Make room public searchable", "Rendre le salon public"}.
{"Make participants list public", "Rendre la liste des participants publique"}.
{"Make room password protected", "Protéger le salon par mot de passe"}.
{"Make room members-only", "Réserver le salon aux membres uniquement"}.
{"Default users as participants", "Les utilisateurs sont par défaut participant"}.
{"Allow users to change subject", "Autoriser les utilisateurs à changer le sujet"}.
{"Allow users to send private messages", "Autoriser les utilisateurs à envoyer des messages privés"}.
{"Allow users to query other users", "Permettre aux utilisateurs d'envoyer des requêtes aux autres utilisateurs"}.
{"Allow users to send invites", "Permettre aux utilisateurs d'envoyer des invitations"}.
{"Enable logging", "Activer l'archivage"}.
{"Number of occupants", "Nombre d'occupants"}.
{"Present real JIDs to", "Rendre le JID réel visible pour"}.
{"moderators only", "modérateurs seulement"}.
{"anyone", "tout le monde"}.
{"Traffic rate limit is exceeded", "La limite de trafic a été dépassée"}.
{"Maximum Number of Occupants", "Nombre maximum d'occupants"}.
{"No limit", "Pas de limite"}.
{"~s invites you to the room ~s", "~s vous a invité dans la salle de discussion ~s"}.
{"the password is", "le mot de passe est"}.

% mod_irc/mod_irc.erl
{"ejabberd IRC module", "Module IRC ejabberd"}.
{"You need an x:data capable client to configure mod_irc settings", "Vous avez besoin d'un client supportant x:data pour configurer le module IRC"}.
{"Registration in mod_irc for ", "Enregistrement du mod_irc pour "}.
{"Enter username and encodings you wish to use for connecting to IRC servers", "Entrez le nom d'utilisateur et les encodages que vous souhaitez utiliser pour vous connecter aux serveurs IRC"}.
{"IRC Username", "Nom d'utilisateur IRC"}.
{"If you want to specify different encodings for IRC servers, fill this list with values in format '{\"irc server\", \"encoding\"}'.  By default this service use \"~s\" encoding.", "Si vous voulez préciser différents encodages pour les serveurs IRC, remplissez cette liste avec des valeurs dans le format '{\"serveur irc\", \"encodage\"}'. Par défaut ce service utilise l'encodage \"~s\"."}.
{"Example: [{\"irc.lucky.net\", \"koi8-r\"}, {\"vendetta.fef.net\", \"iso8859-1\"}].", "Exemple: [{\"irc.lucky.net\", \"koi8-r\"}, {\"vendetta.fef.net\", \"iso8859-1\"}]."}.
{"Encodings", "Encodages"}.
{"IRC Transport", "Passerelle IRC"}.

% web/ejabberd_web_admin.erl
{"ejabberd Web Admin", "Console Web d'administration de ejabberd"}.
{"Administration", "Administration"}.
{"Users", "Utilisateurs"}.
{"Nodes", "Noeuds"}.
{"Statistics", "Statistiques"}.
{"(Raw)", "(Brut)"}.
{"Submitted", "Soumis"}.
{"Bad format", "Mauvais format"}.
{"Raw", "Brut"}.
{"Delete Selected", "Suppression des éléments sélectionnés"}.
{"Submit", "Soumettre"}.
{"~s access rule configuration", "Configuration des règles d'accès ~s"}.
{"Node not found", "Noeud non trouvé"}.
{"Add New", "Ajouter"}.
{"Registered Users", "Utilisateurs enregistrés"}.
{"Registered Users:", "Utilisateurs enregistrés:"}.
{"Change Password", "Modifier le mot de passe"}.
{"Connected Resources:", "Ressources connectées:"}.
{"Password:", "Mot de passe:"}.
{"None", "Aucun"}.
{"Node ", "Noeud "}.
{"Listened Ports", "Ports ouverts"}.
{"Restart", "Redémarrer"}.
{"Stop", "Arrêter"}.
{"RPC Call Error", "Erreur d'appel RPC"}.
{"Name", "Nom"}.
{"Storage Type", "Type de stockage"}.
{"Size", "Taille"}.
{"Memory", "Mémoire"}.
{"OK", "OK"}.
{"Listened Ports at ", "Ports ouverts sur "}.
{"Uptime:", "Temps depuis le démarrage:"}.
{"CPU Time:", "Temps CPU:"}.
{"Transactions Commited:", "Transactions commitées:"}.
{"Transactions Aborted:", "Transactions annulées:"}.
{"Transactions Restarted:", "Transactions redémarrées:"}.
{"Transactions Logged:", "Transactions journalisées:"}.
{"Port", "Port"}.
{"Module", "Module"}.
{"Options", "Options"}.
{"Update", "Mettre à jour"}.
{"Delete", "Supprimer"}.
{"Add User", "Ajouter un utilisateur"}.
{"Offline Messages", "Messages en attente"}.
{"Users Last Activity", "Dernière activité des utilisateurs"}.
{"Never", "Jamais"}.
{"~s's Offline Messages Queue", "~s messages en file d'attente"}.
{"Time", "Heure"}.
{"From", "De"}.
{"To", "A"}.
{"Packet", "Paquet"}.
{"Offline Messages:", "Messages en attente:"}.
{"Roster", "Liste de contacts"}.
{"Nickname", "Pseudo"}.
{"Subscription", "Abonnement"}.
{"Pending", "En suspend"}.
{"Groups", "Groupes"}.
{"Remove", "Enlever"}.
{"Add Jabber ID", "Ajouter un Jabber ID"}.
{"User ", "Utilisateur "}.
{"Roster of ", "Liste de contact de "}.
{"Online", "En ligne"}.
{"Validate", "Valider"}.
{"Name:", "Nom:"}.
{"Description:", "Description:"}.
{"Members:", "Membres:"}.
{"Displayed Groups:", "Groupes affichés:"}.
{"Group ", "Groupe "}.
{"Period: ", "Période:"}.
{"Last month", "Dernier mois"}.
{"Last year", "Dernière année"}.
{"All activity", "Toute activité"}.
{"Show Ordinary Table", "Montrer la table ordinaire"}.
{"Show Integral Table", "Montrer la table intégralement"}.
{"Modules at ", "Modules sur "}.
{"Start", "Démarrer"}.
{"Virtual Hosts", "Serveurs virtuels"}.
{"ejabberd virtual hosts", "Serveurs virtuels d'ejabberd"}.
{"Host", "Serveur"}.
{"No Data", "Aucune information disponible"}.
{"Online Users:", "Utilisateurs connectés:"}.
{"Outgoing s2s Connections:", "Connexions s2s sortantes:"}.
{"Outgoing s2s Servers:", "Serveurs s2s sortants"}.
{"Database Tables at ", "Tables de base de données sur "}.
{"Backup of ", "Sauvegarde de "}.
{"Remark that these options will only backup the builtin Mnesia database. If you are using the ODBC module, you also need to backup your SQL database separately.", "Ces options sauvegardent uniquement la base de données interne Mnesia. Si vous utilisez le module ODBC vous devez sauvegarde votre base SQL séparément."}.
{"Store binary backup:", "Sauvegarde binaire:"}.
{"Restore binary backup immediately:", "Restauration immédiate d'une sauvegarde binaire:"}.
{"Restore binary backup after next ejabberd restart (requires less memory):", "Restauration de la sauvegarde binaire après redémarrage (nécessite moins de mémoire):"}.
{"Store plain text backup:", "Sauvegarde texte:"}.
{"Restore plain text backup immediately:", "Restauration immédiate d'une sauvegarde texte:"}.
{"Statistics of ~p", "Statistiques de ~p"}.
{"Update ", "Mise à jour "}.
{"Update plan", "Plan de mise à jour"}.
{"Updated modules", "Modules mis à jours"}.
{"Update script", "Script de mise à jour"}.
{"Low level update script", "Script de mise à jour de bas-niveau"}.
{"Script check", "Validation du script"}.
{"Shared Roster Groups", "Groupes de liste de contacts partagée"}.
{"Last Activity", "Dernière Activité"}.

% mod_proxy65_service.erl
{"ejabberd SOCKS5 Bytestreams module", "ejabberd SOCKS5 Bytestreams module"}.

% mod_offline_odbc.erl
{"Your contact offline message queue is full. The message has been discarded.", "La file d'attente de message de votre contact est pleine. Votre message a été détruit."}.

% Local Variables:
% mode: erlang
% End:
% vim: set filetype=erlang tabstop=8:
