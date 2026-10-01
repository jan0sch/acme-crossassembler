// ACME - a crossassembler for producing 6502/65c02/65816/65ce02 code.
// Copyright (C) 1998-2026 Marco Baye
// Have a look at "acme.c" for further info
//
// section stuff
#ifndef section_H
#define section_H


#include "config.h"


// constants
#define SCOPE_GLOBAL	0	// number of "global zone"


// "section" structure type definition
struct section {
	scope_t		local_scope;	// section's scope for local symbols
	const char	*type;	// "Zone", "Subzone" or "Macro"
	char		*title;	// zone title, subzone title or macro title
	// CAUTION, only access via section_cheap_scope():
	scope_t		cheap_scope;	// section's scope for cheap locals
};


// current section structure
extern struct section	*section_now;


// create and return new scope
extern scope_t section_new_scope(void);

// write given info into given structure and activate it, making a new scope for locals
extern void section_new(struct section *section, const char *type, char *title);
// write given info into given structure and activate it, using the given scope for locals
extern void section_new_force_scope(struct section *section, const char *type, char *title, scope_t local_scope);

// change scope of cheap locals in given section
extern void section_new_cheap_scope(struct section *section);

// return current scope for cheap locals
extern scope_t section_cheap_scope(void);

// setup outermost section
extern void section_passinit(void);

// create debugging output
extern void section_debug(void);

// lookup zone title (must be held in DynaBuf) and return scope.
// if "title" is non-NULL, permanent pointer to title string will be stored there.
extern scope_t section_zone_title_to_scope(char **title, scope_t scope);

// clear zone forest (for external tools)
extern void section_reinit(void);


#endif
