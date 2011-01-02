/*
 * mattias@sudac.org
 */

#ifndef __IRC_H__
#define __IRC_H__

#include <glib.h>


typedef struct irc_connection irc_connection;
typedef void (*irc_callback)(irc_connection *connection, char *command, GString **parameters, int n);

struct irc_connection
{
    GHashTable *callbacks;
    GIOChannel *io_channel;
    irc_callback default_callback;
};


irc_connection *irc_alloc();
void irc_free(irc_connection *connection);
int irc_connect(irc_connection *connection, char *hostname, int port, char *password, char *nickname, char *username, char *realname);
void irc_disconnect(irc_connection *connection);
void irc_loop(irc_connection *connection);
void irc_define_callback(irc_connection *connection, char *command, irc_callback function); /* command = NULL, sets default callback */
int irc_send(irc_connection *connection, char *format, ...);
void irc_debug_callback(irc_connection *connection, GString **p, int n);
char *irc_prefix_nickname(char *s); /* will probably change input string */
int irc_is_mode(char c);


/* numeric reply constants, generated from: */
/* http://www.alien.net.au/irc/irc2numerics.def */

#define RPL_WELCOME                              "001" /* :Welcome to the Internet Relay Network <nick>!<user>@<host> */
#define RPL_YOURHOST                             "002" /* :Your host is <servername>, running version <version> */
#define RPL_CREATED                              "003" /* :This server was created <date> */
#define RPL_MYINFO                               "004" /* <server_name> <version> <user_modes> <chan_modes> */
#define RPL_MYINFO                               "004" /* <server_name> <version> <user_modes> <chan_modes> <channel_modes_with_params> <user_modes_with_params> <server_modes> <server_modes_with_params> */
#define RPL_BOUNCE                               "005" /* :Try server <server_name>, port <port_number> */
#define RPL_ISUPPORT                             "005"
#define RPL_MAP_UNREAL                           "006"
#define RPL_MAPEND_UNREAL                        "007"
#define RPL_SNOMASK                              "008"
#define RPL_STATMEMTOT                           "009"
#define RPL_STATMEM_IRCU                         "010"
#define RPL_REDIR_                               "010" /* <hostname> <port> :<info> */
#define RPL_YOURCOOKIE                           "014"
#define RPL_MAP_IRCU                             "015"
#define RPL_MAPMORE_IRCU                         "016"
#define RPL_MAPEND_IRCU                          "017"
#define RPL_YOURID                               "042"
#define RPL_SAVENICK                             "043" /* :<info> */
#define RPL_ATTEMPTINGJUNC                       "050"
#define RPL_ATTEMPTINGREROUTE                    "051"
#define RPL_TRACELINK                            "200" /* Link <version>[.<debug_level>] <destination> <next_server> [V<protocol_version> <link_uptime_in_seconds> <backstream_sendq> <upstream_sendq>] */
#define RPL_TRACECONNECTING                      "201" /* Try. <class> <server> */
#define RPL_TRACEHANDSHAKE                       "202" /* H.S. <class> <server> */
#define RPL_TRACEUNKNOWN                         "203" /* ???? <class> [<connection_address>] */
#define RPL_TRACEOPERATOR                        "204" /* Oper <class> <nick> */
#define RPL_TRACEUSER                            "205" /* User <class> <nick> */
#define RPL_TRACESERVER                          "206" /* Serv <class> <int>S <int>C <server> <nick!user|*!*>@<host|server> [V<protocol_version>] */
#define RPL_TRACESERVICE                         "207" /* Service <class> <name> <type> <active_type> */
#define RPL_TRACENEWTYPE                         "208" /* <newtype> 0 <client_name> */
#define RPL_TRACECLASS                           "209" /* Class <class> <count> */
#define RPL_TRACERECONNECT                       "210"
#define RPL_STATS                                "210"
#define RPL_STATSLINKINFO                        "211" /* <linkname> <sendq> <sent_msgs> <sent_bytes> <recvd_msgs> <rcvd_bytes> <time_open> */
#define RPL_STATSCOMMANDS                        "212" /* <command> <count> [<byte_count> <remote_count>] */
#define RPL_STATSCLINE                           "213" /* C <host> * <name> <port> <class> */
#define RPL_STATSNLINE_RFC1459                   "214" /* N <host> * <name> <port> <class> */
#define RPL_STATSILINE                           "215" /* I <host> * <host> <port> <class> */
#define RPL_STATSKLINE                           "216" /* K <host> * <username> <port> <class> */
#define RPL_STATSQLINE_RFC1459                   "217"
#define RPL_STATSPLINE_IRCU                      "217"
#define RPL_STATSYLINE                           "218" /* Y <class> <ping_freq> <connect_freq> <max_sendq> */
#define RPL_ENDOFSTATS                           "219" /* <query> :<info> */
#define RPL_STATSPLINE_HYBRID                    "220"
#define RPL_STATSBLINE_BAHAMUT__UNREAL           "220"
#define RPL_UMODEIS                              "221" /* <user_modes> [<user_mode_params>] */
#define RPL_MODLIST_                             "222"
#define RPL_SQLINE_NICK_UNREAL                   "222"
#define RPL_STATSBLINE_BAHAMUT                   "222"
#define RPL_STATSELINE_BAHAMUT                   "223"
#define RPL_STATSGLINE_UNREAL                    "223"
#define RPL_STATSFLINE_HYBRID__BAHAMUT           "224"
#define RPL_STATSTLINE_UNREAL                    "224"
#define RPL_STATSDLINE_HYBRID                    "225"
#define RPL_STATSZLINE_BAHAMUT                   "225"
#define RPL_STATSELINE_UNREAL                    "225"
#define RPL_STATSCOUNT_BAHAMUT                   "226"
#define RPL_STATSNLINE_UNREAL                    "226"
#define RPL_STATSGLINE_BAHAMUT                   "227"
#define RPL_STATSVLINE_UNREAL                    "227"
#define RPL_STATSQLINE_IRCU                      "228"
#define RPL_SERVICEINFO                          "231"
#define RPL_ENDOFSERVICES                        "232"
#define RPL_RULES_UNREAL                         "232"
#define RPL_SERVICE                              "233"
#define RPL_SERVLIST                             "234" /* <name> <server> <mask> <type> <hopcount> <info> */
#define RPL_SERVLISTEND                          "235" /* <mask> <type> :<info> */
#define RPL_STATSVERBOSE                         "236"
#define RPL_STATSENGINE                          "237"
#define RPL_STATSFLINE_IRCU                      "238"
#define RPL_STATSIAUTH                           "239"
#define RPL_STATSVLINE_RFC2812                   "240"
#define RPL_STATSXLINE_AUSTHEX                   "240"
#define RPL_STATSLLINE                           "241" /* L <hostmask> * <servername> <maxdepth> */
#define RPL_STATSUPTIME                          "242" /* :Server Up <days> days <hours>:<minutes>:<seconds> */
#define RPL_STATSOLINE                           "243" /* O <hostmask> * <nick> [:<info>] */
#define RPL_STATSHLINE                           "244" /* H <hostmask> * <servername> */
#define RPL_STATSSLINE                           "245"
#define RPL_STATSPING                            "246"
#define RPL_STATSTLINE_IRCU                      "246"
#define RPL_STATSULINE_HYBRID                    "246"
#define RPL_STATSBLINE_RFC2812                   "247"
#define RPL_STATSXLINE_HYBRID__PTLINK__UNREAL    "247"
#define RPL_STATSGLINE_IRCU                      "247"
#define RPL_STATSULINE_IRCU                      "248"
#define RPL_STATSDEFINE_IRCNET                   "248"
#define RPL_STATSULINE_                          "249"
#define RPL_STATSDEBUG_HYBRID                    "249"
#define RPL_STATSDLINE_RFC2812                   "250"
#define RPL_STATSCONN                            "250"
#define RPL_LUSERCLIENT                          "251" /* :There are <int> users and <int> invisible on <int> servers */
#define RPL_LUSEROP                              "252" /* <int> :<info> */
#define RPL_LUSERUNKNOWN                         "253" /* <int> :<info> */
#define RPL_LUSERCHANNELS                        "254" /* <int> :<info> */
#define RPL_LUSERME                              "255" /* :I have <int> clients and <int> servers */
#define RPL_ADMINME                              "256" /* <server> :<info> */
#define RPL_ADMINLOC1                            "257" /* :<admin_location> */
#define RPL_ADMINLOC2                            "258" /* :<admin_location> */
#define RPL_ADMINEMAIL                           "259" /* :<email_address> */
#define RPL_TRACELOG                             "261" /* File <logfile> <debug_level> */
#define RPL_TRACEPING_                           "262"
#define RPL_TRACEEND_RFC2812                     "262" /* <server_name> <version>[.<debug_level>] :<info> */
#define RPL_TRYAGAIN                             "263" /* <command> :<info> */
#define RPL_LOCALUSERS                           "265"
#define RPL_GLOBALUSERS                          "266"
#define RPL_START_NETSTAT                        "267"
#define RPL_NETSTAT                              "268"
#define RPL_END_NETSTAT                          "269"
#define RPL_PRIVS                                "270"
#define RPL_SILELIST                             "271"
#define RPL_ENDOFSILELIST                        "272"
#define RPL_NOTIFY                               "273"
#define RPL_ENDNOTIFY_AIRCD                      "274"
#define RPL_STATSDELTA_IRCNET                    "274"
#define RPL_STATSDLINE_IRCU__ULTIMATE            "275"
#define RPL_VCHANEXIST                           "276"
#define RPL_VCHANLIST                            "277"
#define RPL_VCHANHELP                            "278"
#define RPL_GLIST                                "280"
#define RPL_ENDOFGLIST_UNDERNET                  "281"
#define RPL_ACCEPTLIST_UNDERNET                  "281"
#define RPL_ENDOFACCEPT_                         "282"
#define RPL_JUPELIST_UNDERNET                    "282"
#define RPL_ALIST_                               "283"
#define RPL_ENDOFJUPELIST_UNDERNET               "283"
#define RPL_ENDOFALIST_                          "284"
#define RPL_FEATURE_QUAKENET                     "284"
#define RPL_GLIST_HASH_                          "285"
#define RPL_CHANINFO_HANDLE_AIRCD                "285"
#define RPL_NEWHOSTIS_QUAKENET                   "285"
#define RPL_CHANINFO_USERS_AIRCD                 "286"
#define RPL_CHKHEAD_QUAKENET                     "286"
#define RPL_CHANINFO_CHOPS_AIRCD                 "287"
#define RPL_CHANUSER_QUAKENET                    "287"
#define RPL_CHANINFO_VOICES_AIRCD                "288"
#define RPL_PATCHHEAD_QUAKENET                   "288"
#define RPL_CHANINFO_AWAY_AIRCD                  "289"
#define RPL_PATCHCON_QUAKENET                    "289"
#define RPL_CHANINFO_OPERS_AIRCD                 "290"
#define RPL_HELPHDR_UNREAL                       "290"
#define RPL_DATASTR_QUAKENET                     "290"
#define RPL_CHANINFO_BANNED_AIRCD                "291"
#define RPL_HELPOP_UNREAL                        "291"
#define RPL_ENDOFCHECK_QUAKENET                  "291"
#define RPL_CHANINFO_BANS_AIRCD                  "292"
#define RPL_HELPTLR_UNREAL                       "292"
#define RPL_CHANINFO_INVITE_AIRCD                "293"
#define RPL_HELPHLP_UNREAL                       "293"
#define RPL_CHANINFO_INVITES_AIRCD               "294"
#define RPL_HELPFWD_UNREAL                       "294"
#define RPL_CHANINFO_KICK_AIRCD                  "295"
#define RPL_HELPIGN_UNREAL                       "295"
#define RPL_CHANINFO_KICKS                       "296"
#define RPL_END_CHANINFO                         "299"
#define RPL_NONE                                 "300"
#define RPL_AWAY                                 "301" /* <nick> :<message> */
#define RPL_AWAY                                 "301" /* <nick> <seconds away> :<message> */
#define RPL_USERHOST                             "302" /* :*1<reply> *( ' ' <reply> ) */
#define RPL_ISON                                 "303" /* :*1<nick> *( ' ' <nick> ) */
#define RPL_TEXT                                 "304"
#define RPL_UNAWAY                               "305" /* :<info> */
#define RPL_NOWAWAY                              "306" /* :<info> */
#define RPL_USERIP_                              "307"
#define RPL_WHOISREGNICK_BAHAMUT__UNREAL         "307"
#define RPL_SUSERHOST_AUSTHEX                    "307"
#define RPL_NOTIFYACTION_AIRCD                   "308"
#define RPL_WHOISADMIN_BAHAMUT                   "308"
#define RPL_RULESSTART_UNREAL                    "308"
#define RPL_NICKTRACE_AIRCD                      "309"
#define RPL_WHOISSADMIN_BAHAMUT                  "309"
#define RPL_ENDOFRULES_UNREAL                    "309"
#define RPL_WHOISHELPER_AUSTHEX                  "309"
#define RPL_WHOISSVCMSG_BAHAMUT                  "310"
#define RPL_WHOISHELPOP_UNREAL                   "310"
#define RPL_WHOISSERVICE_AUSTHEX                 "310"
#define RPL_WHOISUSER                            "311" /* <nick> <user> <host> * :<real_name> */
#define RPL_WHOISSERVER                          "312" /* <nick> <server> :<server_info> */
#define RPL_WHOISOPERATOR                        "313" /* <nick> :<privileges> */
#define RPL_WHOWASUSER                           "314" /* <nick> <user> <host> * :<real_name> */
#define RPL_ENDOFWHO                             "315" /* <name> :<info> */
#define RPL_WHOISCHANOP                          "316"
#define RPL_WHOISIDLE                            "317" /* <nick> <seconds> :seconds idle */
#define RPL_ENDOFWHOIS                           "318" /* <nick> :<info> */
#define RPL_WHOISCHANNELS                        "319" /* <nick> :*( ( '@' / '+' ) <channel> ' ' ) */
#define RPL_WHOISVIRT                            "320"
#define RPL_WHOIS_HIDDEN                         "320"
#define RPL_WHOISSPECIAL                         "320"
#define RPL_LISTSTART                            "321" /* Channels :Users  Name */
#define RPL_LIST                                 "322" /* <channel> <#_visible> :<topic> */
#define RPL_LISTEND                              "323" /* :<info> */
#define RPL_CHANNELMODEIS                        "324" /* <channel> <mode> <mode_params> */
#define RPL_UNIQOPIS_RFC2812                     "325" /* <channel> <nickname> */
#define RPL_CHANNELPASSIS_                       "325"
#define RPL_NOCHANPASS                           "326"
#define RPL_CHPASSUNKNOWN                        "327"
#define RPL_CHANNEL_URL                          "328"
#define RPL_CREATIONTIME                         "329"
#define RPL_WHOWAS_TIME_                         "330"
#define RPL_WHOISACCOUNT_                        "330"
#define RPL_NOTOPIC                              "331" /* <channel> :<info> */
#define RPL_TOPIC                                "332" /* <channel> :<topic> */
#define RPL_TOPICWHOTIME                         "333"
#define RPL_LISTUSAGE_IRCU                       "334"
#define RPL_COMMANDSYNTAX_BAHAMUT                "334"
#define RPL_LISTSYNTAX_UNREAL                    "334"
#define RPL_WHOISBOT_UNREAL                      "335"
#define RPL_CHANPASSOK_                          "338"
#define RPL_WHOISACTUALLY_IRCU__BAHAMUT          "338"
#define RPL_BADCHANPASS                          "339"
#define RPL_USERIP                               "340"
#define RPL_INVITING                             "341" /* <nick> <channel> */
#define RPL_SUMMONING                            "342" /* <user> :<info> */
#define RPL_INVITELIST                           "346" /* <channel> <invitemask> */
#define RPL_ENDOFINVITELIST                      "347" /* <channel> :<info> */
#define RPL_EXCEPTLIST                           "348" /* <channel> <exceptionmask> */
#define RPL_ENDOFEXCEPTLIST                      "349" /* <channel> :<info> */
#define RPL_VERSION                              "351" /* <version>[.<debuglevel>] <server> :<comments> */
#define RPL_WHOREPLY                             "352" /* <channel> <user> <host> <server> <nick> <H|G>[*][@|+] :<hopcount> <real_name> */
#define RPL_NAMREPLY                             "353" /* ( '=' / '*' / '@' ) <channel> ' ' : [ '@' / '+' ] <nick> *( ' ' [ '@' / '+' ] <nick> ) */
#define RPL_WHOSPCRPL                            "354"
#define RPL_NAMREPLY_                            "355" /* ( '=' / '*' / '@' ) <channel> ' ' : [ '@' / '+' ] <nick> *( ' ' [ '@' / '+' ] <nick> ) */
#define RPL_MAP_AUSTHEX                          "357"
#define RPL_MAPMORE_AUSTHEX                      "358"
#define RPL_MAPEND_AUSTHEX                       "359"
#define RPL_KILLDONE                             "361"
#define RPL_CLOSING                              "362"
#define RPL_CLOSEEND                             "363"
#define RPL_LINKS                                "364" /* <mask> <server> :<hopcount> <server_info> */
#define RPL_ENDOFLINKS                           "365" /* <mask> :<info> */
#define RPL_ENDOFNAMES                           "366" /* <channel> :<info> */
#define RPL_BANLIST                              "367" /* <channel> <banid> [<time_left> :<reason>] */
#define RPL_ENDOFBANLIST                         "368" /* <channel> :<info> */
#define RPL_ENDOFWHOWAS                          "369" /* <nick> :<info> */
#define RPL_INFO                                 "371" /* :<string> */
#define RPL_MOTD                                 "372" /* :- <string> */
#define RPL_INFOSTART                            "373"
#define RPL_ENDOFINFO                            "374" /* :<info> */
#define RPL_MOTDSTART                            "375" /* :- <server> Message of the day - */
#define RPL_ENDOFMOTD                            "376" /* :<info> */
#define RPL_KICKEXPIRED_AIRCD                    "377"
#define RPL_SPAM                                 "377" /* :<text> */
#define RPL_BANEXPIRED_AIRCD                     "378"
#define RPL_WHOISHOST_UNREAL                     "378"
#define RPL_KICKLINKED_AIRCD                     "379"
#define RPL_WHOISMODES_UNREAL                    "379"
#define RPL_BANLINKED_AIRCD                      "380"
#define RPL_YOURHELPER_AUSTHEX                   "380"
#define RPL_YOUREOPER                            "381" /* :<info> */
#define RPL_REHASHING                            "382" /* <config_file> :<info> */
#define RPL_YOURESERVICE                         "383" /* :You are service <service_name> */
#define RPL_MYPORTIS                             "384"
#define RPL_NOTOPERANYMORE                       "385"
#define RPL_QLIST_UNREAL                         "386"
#define RPL_IRCOPS_ULTIMATE                      "386"
#define RPL_ENDOFQLIST_UNREAL                    "387"
#define RPL_ENDOFIRCOPS_ULTIMATE                 "387"
#define RPL_ALIST                                "388"
#define RPL_ENDOFALIST                           "389"
#define RPL_TIME                                 "391" /* <server> :<time string> */
#define RPL_TIME_IRCU                            "391" /* <server> <timestamp> <offset> :<time string> */
#define RPL_TIME_BDQ_IRCD                        "391" /* <server> <timezone name> <microseconds> :<time string> */
#define RPL_TIME_                                "391" /* <server> <year> <month> <day> <hour> <minute> <second> */
#define RPL_USERSSTART                           "392" /* :UserID   Terminal  Host */
#define RPL_USERS                                "393" /* :<username> <ttyline> <hostname> */
#define RPL_ENDOFUSERS                           "394" /* :<info> */
#define RPL_NOUSERS                              "395" /* :<info> */
#define RPL_HOSTHIDDEN                           "396"
#define ERR_UNKNOWNERROR                         "400" /* <command> [<?>] :<info> */
#define ERR_NOSUCHNICK                           "401" /* <nick> :<reason> */
#define ERR_NOSUCHSERVER                         "402" /* <server> :<reason> */
#define ERR_NOSUCHCHANNEL                        "403" /* <channel> :<reason> */
#define ERR_CANNOTSENDTOCHAN                     "404" /* <channel> :<reason> */
#define ERR_TOOMANYCHANNELS                      "405" /* <channel> :<reason> */
#define ERR_WASNOSUCHNICK                        "406" /* <nick> :<reason> */
#define ERR_TOOMANYTARGETS                       "407" /* <target> :<reason> */
#define ERR_NOSUCHSERVICE                        "408" /* <service_name> :<reason> */
#define ERR_NOCOLORSONCHAN_BAHAMUT               "408"
#define ERR_NOORIGIN                             "409" /* :<reason> */
#define ERR_NORECIPIENT                          "411" /* :<reason> */
#define ERR_NOTEXTTOSEND                         "412" /* :<reason> */
#define ERR_NOTOPLEVEL                           "413" /* <mask> :<reason> */
#define ERR_WILDTOPLEVEL                         "414" /* <mask> :<reason> */
#define ERR_BADMASK                              "415" /* <mask> :<reason> */
#define ERR_TOOMANYMATCHES                       "416" /* <command> [<mask>] :<info> */
#define ERR_QUERYTOOLONG                         "416"
#define ERR_LENGTHTRUNCATED                      "419"
#define ERR_UNKNOWNCOMMAND                       "421" /* <command> :<reason> */
#define ERR_NOMOTD                               "422" /* :<reason> */
#define ERR_NOADMININFO                          "423" /* <server> :<reason> */
#define ERR_FILEERROR                            "424" /* :<reason> */
#define ERR_NOOPERMOTD                           "425"
#define ERR_TOOMANYAWAY                          "429"
#define ERR_EVENTNICKCHANGE                      "430"
#define ERR_NONICKNAMEGIVEN                      "431" /* :<reason> */
#define ERR_ERRONEUSNICKNAME                     "432" /* <nick> :<reason> */
#define ERR_NICKNAMEINUSE                        "433" /* <nick> :<reason> */
#define ERR_SERVICENAMEINUSE_AUSTHEX_            "434"
#define ERR_NORULES_UNREAL__ULTIMATE             "434"
#define ERR_SERVICECONFUSED_UNREAL               "435"
#define ERR_BANONCHAN_BAHAMUT                    "435"
#define ERR_NICKCOLLISION                        "436" /* <nick> :<reason> */
#define ERR_UNAVAILRESOURCE_RFC2812              "437" /* <nick/channel/service> :<reason> */
#define ERR_BANNICKCHANGE_IRCU                   "437"
#define ERR_NICKTOOFAST_IRCU                     "438"
#define ERR_DEAD_IRCNET                          "438"
#define ERR_TARGETTOOFAST                        "439"
#define ERR_SERVICESDOWN                         "440"
#define ERR_USERNOTINCHANNEL                     "441" /* <nick> <channel> :<reason> */
#define ERR_NOTONCHANNEL                         "442" /* <channel> :<reason> */
#define ERR_USERONCHANNEL                        "443" /* <nick> <channel> [:<reason>] */
#define ERR_NOLOGIN                              "444" /* <user> :<reason> */
#define ERR_SUMMONDISABLED                       "445" /* :<reason> */
#define ERR_USERSDISABLED                        "446" /* :<reason> */
#define ERR_NONICKCHANGE                         "447"
#define ERR_NOTIMPLEMENTED                       "449" /* Unspecified */
#define ERR_NOTREGISTERED                        "451" /* :<reason> */
#define ERR_IDCOLLISION                          "452"
#define ERR_NICKLOST                             "453"
#define ERR_HOSTILENAME                          "455"
#define ERR_ACCEPTFULL                           "456"
#define ERR_ACCEPTEXIST                          "457"
#define ERR_ACCEPTNOT                            "458"
#define ERR_NOHIDING                             "459"
#define ERR_NOTFORHALFOPS                        "460"
#define ERR_NEEDMOREPARAMS                       "461" /* <command> :<reason> */
#define ERR_ALREADYREGISTERED                    "462" /* :<reason> */
#define ERR_NOPERMFORHOST                        "463" /* :<reason> */
#define ERR_PASSWDMISMATCH                       "464" /* :<reason> */
#define ERR_YOUREBANNEDCREEP                     "465" /* :<reason> */
#define ERR_YOUWILLBEBANNED                      "466"
#define ERR_KEYSET                               "467" /* <channel> :<reason> */
#define ERR_INVALIDUSERNAME_IRCU                 "468"
#define ERR_ONLYSERVERSCANCHANGE_BAHAMUT__UNREAL "468"
#define ERR_LINKSET                              "469"
#define ERR_LINKCHANNEL_UNREAL                   "470"
#define ERR_KICKEDFROMCHAN_AIRCD                 "470"
#define ERR_CHANNELISFULL                        "471" /* <channel> :<reason> */
#define ERR_UNKNOWNMODE                          "472" /* <char> :<reason> */
#define ERR_INVITEONLYCHAN                       "473" /* <channel> :<reason> */
#define ERR_BANNEDFROMCHAN                       "474" /* <channel> :<reason> */
#define ERR_BADCHANNELKEY                        "475" /* <channel> :<reason> */
#define ERR_BADCHANMASK                          "476" /* <channel> :<reason> */
#define ERR_NOCHANMODES_RFC2812                  "477" /* <channel> :<reason> */
#define ERR_NEEDREGGEDNICK_BAHAMUT__IRCU__UNREAL "477"
#define ERR_BANLISTFULL                          "478" /* <channel> <char> :<reason> */
#define ERR_BADCHANNAME                          "479"
#define ERR_LINKFAIL                             "479"
#define ERR_NOULINE_AUSTHEX                      "480"
#define ERR_CANNOTKNOCK_UNREAL                   "480"
#define ERR_NOPRIVILEGES                         "481" /* :<reason> */
#define ERR_CHANOPRIVSNEEDED                     "482" /* <channel> :<reason> */
#define ERR_CANTKILLSERVER                       "483" /* :<reason> */
#define ERR_RESTRICTED_RFC2812                   "484" /* :<reason> */
#define ERR_ISCHANSERVICE_UNDERNET               "484"
#define ERR_DESYNC_BAHAMUT__HYBRID__PTLINK       "484"
#define ERR_ATTACKDENY_UNREAL                    "484"
#define ERR_UNIQOPRIVSNEEDED                     "485" /* :<reason> */
#define ERR_KILLDENY_UNREAL                      "485"
#define ERR_CANTKICKADMIN_PTLINK                 "485"
#define ERR_ISREALSERVICE_QUAKENET               "485"
#define ERR_NONONREG_                            "486"
#define ERR_HTMDISABLED_UNREAL                   "486"
#define ERR_ACCOUNTONLY_QUAKENET                 "486"
#define ERR_CHANTOORECENT_IRCNET                 "487"
#define ERR_MSGSERVICES_BAHAMUT                  "487"
#define ERR_TSLESSCHAN                           "488"
#define ERR_VOICENEEDED_UNDERNET                 "489"
#define ERR_SECUREONLYCHAN_UNREAL                "489"
#define ERR_NOOPERHOST                           "491" /* :<reason> */
#define ERR_NOSERVICEHOST                        "492"
#define ERR_NOFEATURE                            "493"
#define ERR_BADFEATURE                           "494"
#define ERR_BADLOGTYPE                           "495"
#define ERR_BADLOGSYS                            "496"
#define ERR_BADLOGVALUE                          "497"
#define ERR_ISOPERLCHAN                          "498"
#define ERR_UMODEUNKNOWNFLAG                     "501" /* :<reason> */
#define ERR_USERSDONTMATCH                       "502" /* :<reason> */
#define ERR_GHOSTEDCLIENT                        "503"
#define ERR_VWORLDWARN                           "503" /* :<warning_text> */
#define ERR_USERNOTONSERV                        "504"
#define ERR_SILELISTFULL                         "511"
#define ERR_TOOMANYWATCH                         "512"
#define ERR_BADPING                              "513"
#define ERR_INVALID_ERROR_IRCU                   "514"
#define ERR_TOOMANYDCC_BAHAMUT                   "514"
#define ERR_BADEXPIRE                            "515"
#define ERR_DONTCHEAT                            "516"
#define ERR_DISABLED                             "517" /* <command> :<info/reason> */
#define ERR_NOINVITE_UNREAL                      "518"
#define ERR_LONGMASK_IRCU                        "518"
#define ERR_ADMONLY_UNREAL                       "519"
#define ERR_TOOMANYUSERS_IRCU                    "519"
#define ERR_OPERONLY_UNREAL                      "520"
#define ERR_MASKTOOWIDE_IRCU                     "520"
#define ERR_WHOTRUNC                             "520"
#define ERR_LISTSYNTAX_BAHAMUT                   "521"
#define ERR_WHOSYNTAX                            "522"
#define ERR_WHOLIMEXCEED                         "523"
#define ERR_QUARANTINED_IRCU                     "524"
#define ERR_OPERSPVERIFY_UNREAL                  "524"
#define ERR_REMOTEPFX                            "525" /* <nickname> :<reason> */
#define ERR_PFXUNROUTABLE                        "526" /* <nickname> :<reason> */
#define ERR_BADHOSTMASK                          "550"
#define ERR_HOSTUNAVAIL                          "551"
#define ERR_USINGSLINE                           "552"
#define ERR_STATSSLINE_QUAKENET                  "553"
#define RPL_LOGON                                "600"
#define RPL_LOGOFF                               "601"
#define RPL_WATCHOFF                             "602"
#define RPL_WATCHSTAT                            "603"
#define RPL_NOWON                                "604"
#define RPL_NOWOFF                               "605"
#define RPL_WATCHLIST                            "606"
#define RPL_ENDOFWATCHLIST                       "607"
#define RPL_WATCHCLEAR                           "608"
#define RPL_MAPMORE_UNREAL                       "610"
#define RPL_ISOPER_ULTIMATE                      "610"
#define RPL_ISLOCOP                              "611"
#define RPL_ISNOTOPER                            "612"
#define RPL_ENDOFISOPER                          "613"
#define RPL_MAPMORE_PTLINK                       "615"
#define RPL_WHOISMODES_ULTIMATE                  "615"
#define RPL_WHOISHOST_ULTIMATE                   "616"
#define RPL_DCCSTATUS                            "617"
#define RPL_WHOISBOT_ULTIMATE                    "617"
#define RPL_DCCLIST                              "618"
#define RPL_ENDOFDCCLIST_BAHAMUT                 "619"
#define RPL_WHOWASHOST_ULTIMATE                  "619"
#define RPL_DCCINFO_BAHAMUT                      "620"
#define RPL_RULESSTART_ULTIMATE                  "620"
#define RPL_RULES_ULTIMATE                       "621"
#define RPL_ENDOFRULES_ULTIMATE                  "622"
#define RPL_MAPMORE_ULTIMATE                     "623"
#define RPL_OMOTDSTART                           "624"
#define RPL_OMOTD                                "625"
#define RPL_ENDOFOMOTD                           "626"
#define RPL_SETTINGS                             "630"
#define RPL_ENDOFSETTINGS                        "631"
#define RPL_DUMPING                              "640"
#define RPL_DUMPRPL                              "641"
#define RPL_EODUMP                               "642"
#define RPL_TRACEROUTE_HOP                       "660" /* <target> <hop#> [<address> [<hostname> | '*'] <usec_ping>] */
#define RPL_TRACEROUTE_START                     "661" /* <target> <target_FQDN> <target_address> <max_hops> */
#define RPL_MODECHANGEWARN                       "662" /* ['+' | '-']<mode_char> :<warning> */
#define RPL_CHANREDIR                            "663" /* <old_chan> <new_chan> :<info> */
#define RPL_SERVMODEIS                           "664" /* <server> <modes> <parameters>.. */
#define RPL_OTHERUMODEIS                         "665" /* <nickname> <modes> */
#define RPL_ENDOF_GENERIC                        "666" /* <command> [<parameter> ...] :<info> */
#define RPL_WHOWASDETAILS                        "670" /* <nick> <type> :<information> */
#define RPL_WHOISSECURE                          "671" /* <nick> <type> [:<info>] */
#define RPL_UNKNOWNMODES                         "672" /* <modes> :<info> */
#define RPL_CANNOTSETMODES                       "673" /* <modes> :<info> */
#define RPL_LUSERSTAFF                           "678" /* <staff_online_count> :<info> */
#define RPL_TIMEONSERVERIS                       "679" /* <seconds> [<nanoseconds> | '0'] <timezone> <flags> :<info> */
#define RPL_NETWORKS                             "682" /* <name> <through_name> <hops> :<info> */
#define RPL_YOURLANGUAGEIS                       "687" /* <code(s)> :<info> */
#define RPL_LANGUAGE                             "688" /* <code> <revision> <maintainer> <flags> * :<info> */
#define RPL_WHOISSTAFF                           "689" /* :<info> */
#define RPL_WHOISLANGUAGE                        "690" /* <nick> <language codes> */
#define RPL_TARGUMODEG                           "716" /* <nick> :<info> */
#define RPL_TARGNOTIFY                           "717" /* <nick> :<info> */
#define RPL_UMODEGMSG                            "718" /* <nick> <user>@<host> :<info> */
#define RPL_XINFO                                "771"
#define RPL_XINFOSTART                           "773"
#define RPL_XINFOEND                             "774"
#define ERR_CANNOTCHANGEUMODE                    "973" /* <mode_char> :<reason> */
#define ERR_CANNOTCHANGECHANMODE                 "974" /* <mode_char> :<reason> */
#define ERR_CANNOTCHANGESERVERMODE               "975" /* <mode_char> :<reason> */
#define ERR_CANNOTSENDTONICK                     "976" /* <nick> :<reason> */
#define ERR_UNKNOWNSERVERMODE                    "977" /* <modechar> :<info> */
#define ERR_SERVERMODELOCK                       "979" /* <target> :<info> */
#define ERR_BADCHARENCODING                      "980" /* <command> <charset> :<info> */
#define ERR_TOOMANYLANGUAGES                     "981" /* <max_langs> :<info> */
#define ERR_NOLANGUAGE                           "982" /* <language_code> :<info> */
#define ERR_TEXTTOOSHORT                         "983" /* <command> :<info> */
#define ERR_NUMERIC_ERR                          "999"

#endif

