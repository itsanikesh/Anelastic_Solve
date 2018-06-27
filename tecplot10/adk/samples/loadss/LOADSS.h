/* LOADSS.h */
#define MAX_FILENAME_LEN    5000
#define MAX_INSTRUCTION_LEN 1000
#define MAX_TITLE_LEN       128
#define DEFAULT_TITLE "Converted Spreadsheet Data"

StringList_pa pack_instructions(void);
Boolean_t unpack_instructions(StringList_pa sl);
Boolean_t ask_filename(void);
Boolean_t STDCALL load_text_file(StringList_pa sl);


