#ifndef __IRC_CALLBACKS_H__
#define __IRC_CALLBACKS_H__

#include <glib.h>

#include "irc.h"

void irc_callback_debug(irc_connection *connection, char *command, GString **p, int n);
void irc_callback_talk_push(irc_connection *ic, char *command, GString **p, int n);
void irc_callback_motd_start(irc_connection *ic, char *command, GString **p, int n);
void irc_callback_motd(irc_connection *ic, char *command, GString **p, int n);
void irc_callback_names_reply(irc_connection *ic, char *command, GString **p, int n);
void irc_callback_topic_reply(irc_connection *ic, char *command, GString **p, int n);
void irc_callback_topic(irc_connection *ic, char *command, GString **p, int n);
void irc_callback_ping(irc_connection *ic, char *command, GString **p, int n);
void irc_callback_join(irc_connection *ic, char *command, GString **p, int n);
void irc_callback_part(irc_connection *ic, char *command, GString **p, int n);
void irc_callback_mode(irc_connection *ic, char *command, GString **p, int n);
void irc_callback_kick(irc_connection *ic, char *command, GString **p, int n);

#endif

