#define HKEY_LOCAL_MACHINE  0x80000002
#define HKEY_CLASSES_ROOT   0x80000000
#define HKEY_USERS          0x80000003
#define HKEY_CURRENT_USER   0x80000001
#define HKEY_NO_KEY         0x00000000
#define HKEY_CURRENT_CONFIG 0x80000005
#define HKEY_DYN_DATA       0x80000006

#define ERROR_SUCCESS       0
#define ERROR_NO_MORE_ITEMS 259


// (key) Access rights
#define DELETE                           0x00010000
#define READ_CONTROL                     0x00020000
#define WRITE_DAC                        0x00040000
#define WRITE_OWNER                      0x00080000
#define SYNCHRONIZE                      0x00100000
#define STANDARD_RIGHTS_REQUIRED         0x000F0000
#define STANDARD_RIGHTS_READ             READ_CONTROL
#define STANDARD_RIGHTS_WRITE            READ_CONTROL
#define STANDARD_RIGHTS_EXECUTE          READ_CONTROL
#define STANDARD_RIGHTS_ALL              0x001F0000
#define SPECIFIC_RIGHTS_ALL              0x0000FFFF

#define KEY_QUERY_VALUE         0x00001
#define KEY_SET_VALUE           0x00002
#define KEY_CREATE_SUB_KEY      0x00004
#define KEY_ENUMERATE_SUB_KEYS  0x00008
#define KEY_NOTIFY              0x00010
#define KEY_CREATE_LINK         0x00020

#define KEY_READ          STANDARD_RIGHTS_READ + KEY_QUERY_VALUE + KEY_ENUMERATE_SUB_KEYS + KEY_NOTIFY
#define KEY_EXECUTE       KEY_READ
#define KEY_WRITE         STANDARD_RIGHTS_WRITE + KEY_SET_VALUE + KEY_CREATE_SUB_KEY
#define KEY_ALL_ACCESS    STANDARD_RIGHTS_ALL + KEY_QUERY_VALUE + KEY_SET_VALUE + KEY_CREATE_SUB_KEY + KEY_ENUMERATE_SUB_KEYS + KEY_NOTIFY + KEY_CREATE_LINK


// Registry value type
#define REG_NONE                     0    // No value type
#define REG_SZ                       1    // Unicode nul terminated string
#define REG_EXPAND_SZ                2    // Unicode nul terminated string
                                          // (with environment variable references)
#define REG_BINARY                   3    // Free form binary
#define REG_DWORD                    4    // 32-bit number
#define REG_DWORD_LITTLE_ENDIAN      4    // 32-bit number (same as REG_DWORD)
#define REG_DWORD_BIG_ENDIAN         5    // 32-bit number
#define REG_LINK                     6    // Symbolic Link (unicode)
#define REG_MULTI_SZ                 7    // Multiple Unicode strings
#define REG_RESOURCE_LIST               8    // Resource list in the resource map
#define REG_FULL_RESOURCE_DESCRIPTOR    9   // Resource list in the hardware description
#define REG_RESOURCE_REQUIREMENTS_LIST 10


//
// Open/Create Options
//

#define REG_OPTION_RESERVED         0x00000000   // Parameter is reserved

#define REG_OPTION_NON_VOLATILE     0x00000000   // Key is preserved
                                                 // when system is rebooted

#define REG_OPTION_VOLATILE         0x00000001   // Key is not preserved
                                                 // when system is rebooted

#define REG_OPTION_CREATE_LINK      0x00000002   // Created key is a
                                                 // symbolic link

#define REG_OPTION_BACKUP_RESTORE   0x00000004   // open for backup or restore
                                                 // special access rules
                                                 // privilege required

#define REG_OPTION_OPEN_LINK        0x00000008   // Open symbolic link

//
// Key creation/open disposition
//

#define REG_CREATED_NEW_KEY         0x00000001   // New Registry Key created
#define REG_OPENED_EXISTING_KEY     0x00000002   // Existing Key opened

//
// Key restore flags
//

#define REG_WHOLE_HIVE_VOLATILE     0x00000001   // Restore whole hive volatile
#define REG_REFRESH_HIVE            0x00000002   // Unwind changes to last flush
#define REG_NO_LAZY_FLUSH           0x00000004   // Never lazy flush this hive


