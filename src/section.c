// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2026 Marco Baye
// Have a look at "acme.c" for further info
//
// section stuff (move to symbol.h?)
#include "section.h"
#include <string.h>	// only for memset()
#include "config.h"
#include "dynabuf.h"
#include "global.h"
#include "input.h"
#include "symbol.h"
#include "tree.h"


// constants
#define FIRST_NONGLOBAL_SCOPE	(SCOPE_GLOBAL + 1)


// fake section structure (for error msgs before any real section is in use)
static struct section	initial_section	= {
	SCOPE_GLOBAL,	// local scope value (dummy)
	"during",	// "type"	=> normally "zone Title" or
	"init",		// "title"	=>  "macro test", now "during init"
	SCOPE_GLOBAL,	// cheap scope value (none, but dummy anway)
};


// variables
struct section		*section_now	= &initial_section;	// current section
static struct section	outer_section;	// outermost section struct
static scope_t		next_nonglobal_scope;	// for locals/cheaps


// write given info into given structure and activate it
static void new_section(struct section *section, const char *type, char *title, scope_t local_scope)
{
	// new scope for locals
	section->local_scope = local_scope;
	// keep scope for cheap locals
	section->cheap_scope = section_now->cheap_scope;
	// copy other data
	section->type = type;
	section->title = title;
	// activate new section
	section_now = section;
	//printf("[new section %d: %s, %s]\n", section->local_scope, section->type, section->title);
}


// create and return new scope
scope_t section_new_scope(void)
{
	return next_nonglobal_scope++;
}


// write given info into given structure and activate it, making a new scope for locals
void section_new(struct section *section, const char *type, char *title)
{
	new_section(section, type, title, section_new_scope());
}
// write given info into given structure and activate it, using the given scope for locals
void section_new_force_scope(struct section *section, const char *type, char *title, scope_t local_scope)
{
	new_section(section, type, title, local_scope);
}


// change scope of cheap locals in given section
void section_new_cheap_scope(struct section *section)
{
	// invalidate scope for cheap locals
	section->cheap_scope = SCOPE_GLOBAL;	// invalid value, see fn below for real change
}
// return current scope for cheap locals
scope_t section_cheap_scope(void)
{
	if (section_now->cheap_scope == SCOPE_GLOBAL)
		section_now->cheap_scope = section_new_scope();
	return section_now->cheap_scope;
}


// setup outermost section
void section_passinit(void)
{
//	printf("[old maximum: next_nonglobal_scope=%d]\n", next_nonglobal_scope);
	next_nonglobal_scope = FIRST_NONGLOBAL_SCOPE;
	section_new(&outer_section, "Zone", s_untitled);
	section_new_cheap_scope(&outer_section);
}


// create debugging output
void section_debug(void)
{
	printf("scope counter: %d\n", next_nonglobal_scope);
}


struct zone {
	int	pass_number;
	scope_t	scope;
};
static struct rwnode	*zone_forest[256];	// trees (because of 8b hash)
// lookup zone title (must be held in DynaBuf) and return scope.
// if "title" is non-NULL, permanent pointer to title string will be stored there.
scope_t section_zone_title_to_scope(char **title, scope_t scope)
{
	struct rwnode	*result;
	boolean		created;
	struct zone	*zone;

	// look up title in zone tree. if not found, create:
	created = tree_hard_scan(&result, zone_forest, scope, TRUE);
	if (created) {
		// prepare new tree item
		zone = safe_malloc(sizeof(*zone));
		zone->pass_number = pass.number - 1;	// trigger block below
		result->body = zone;
//		printf("created zone <%s>.\n", result->id_string);
	}
	zone = result->body;
	// we want the same scope numbers in all passes, therefore we call
	// section_new_scope() not in the "if" block above, but exactly once per
	// pass:
	if (zone->pass_number != pass.number) {
		zone->pass_number = pass.number;
		zone->scope = section_new_scope();
	}
	// if wanted, return permanent pointer to title string
	if (title)
		*title = result->id_string;
	return zone->scope;
}


// clear zone forest (for external tools)
void section_reinit(void)
{
	memset(zone_forest, 0, sizeof(zone_forest));
}
