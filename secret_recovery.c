#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>

#define MAX_SHARES 100
#define MAX_VALUE_LENGTH 200
#define MAX_JSON_SIZE 50000

typedef struct {
    int x;
    long long y;
} Share;

typedef struct {
    int n;  // total shares available
    int k;  // minimum shares needed
    Share shares[MAX_SHARES];
    int share_count;
} SecretSharingData;

// Convert character to digit value for any base
int char_to_digit(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'z') return c - 'a' + 10;
    if (c >= 'A' && c <= 'Z') return c - 'A' + 10;
    return -1;
}

// Convert string from given base to decimal
long long base_to_decimal(const char* value_str, int base) {
    if (base < 2 || base > 36) return 0;
    
    long long result = 0;
    int len = strlen(value_str);
    
    for (int i = 0; i < len; i++) {
        int digit = char_to_digit(value_str[i]);
        if (digit == -1 || digit >= base) return 0;
        result = result * base + digit;
    }
    
    return result;
}

// Extract the quoted string that appears AFTER the first colon in the line
void extract_string_after_colon(const char* str, char* result) {
    const char* colon = strchr(str, ':');
    if (!colon) { result[0] = '\0'; return; }
    const char* p = colon + 1;
    while (*p && isspace((unsigned char)*p)) p++;
    if (*p == '"') p++;
    const char* end = p;
    while (*end && *end != '"' && *end != '\n' && *end != '\r') end++;
    int len = (int)(end - p);
    if (len < 0) len = 0;
    if (len >= MAX_VALUE_LENGTH) len = MAX_VALUE_LENGTH - 1;
    strncpy(result, p, len);
    result[len] = '\0';
}

// Extract number after colon
int extract_number_after_colon(const char* str) {
    const char* colon = strchr(str, ':');
    if (!colon) return 0;
    
    colon++;
    while (*colon && (isspace(*colon) || *colon == '"')) colon++;
    
    return atoi(colon);
}

// Parse JSON input dynamically
int parse_json_input(const char* json_str, SecretSharingData* data) {
    data->share_count = 0;
    data->n = 0;
    data->k = 0;
    
    // Extract n and k values
    char* keys_section = strstr(json_str, "\"keys\"");
    if (keys_section) {
        char* n_line = strstr(keys_section, "\"n\"");
        char* k_line = strstr(keys_section, "\"k\"");
        
        if (n_line) data->n = extract_number_after_colon(n_line);
        if (k_line) data->k = extract_number_after_colon(k_line);
    }
    
    // ...existing code...
    
    // Parse each share dynamically
    char* current_pos = (char*)json_str;
    
    // Look for numbered shares (could be any number)
    for (int share_id = 1; share_id <= 100; share_id++) {
        char search_pattern[20];
        sprintf(search_pattern, "\"%d\":", share_id);
        
        char* share_block = strstr(current_pos, search_pattern);
        if (!share_block) continue;
        
        // Find the end of this share block
        char* block_end = share_block;
        int brace_count = 0;
        int in_share = 0;
        
        while (*block_end) {
            if (*block_end == '{') {
                brace_count++;
                in_share = 1;
            } else if (*block_end == '}') {
                brace_count--;
                if (in_share && brace_count == 0) break;
            }
            block_end++;
        }
        
        // Extract base (handle quoted and unquoted)
        char* base_line = strstr(share_block, "\"base\"");
        if (!base_line || base_line > block_end) continue;
        int base = 0;
        char* base_colon = strchr(base_line, ':');
        if (base_colon) {
            base_colon++;
            while (*base_colon && (isspace(*base_colon) || *base_colon == '"')) base_colon++;
            char base_buf[20] = {0};
            int i = 0;
            while (*base_colon && (isdigit(*base_colon) || *base_colon == '"') && i < 19) {
                if (isdigit(*base_colon)) base_buf[i++] = *base_colon;
                base_colon++;
            }
            base_buf[i] = '\0';
            base = atoi(base_buf);
        }

        // Extract value (always quoted)
        char* value_line = strstr(share_block, "\"value\"");
        if (!value_line || value_line > block_end) continue;
        char value_str[MAX_VALUE_LENGTH] = {0};
        extract_string_after_colon(value_line, value_str);

        // Convert to decimal
        long long decimal_value = base_to_decimal(value_str, base);

        // Store the share
        data->shares[data->share_count].x = share_id;
        data->shares[data->share_count].y = decimal_value;
        data->share_count++;

    // ...existing code...
    }
    
    // ...existing code...
    return (data->share_count >= data->k) ? 1 : 0;
}

// Lagrange interpolation to find secret at x=0
long long lagrange_interpolation(Share points[], int k) {
    // ...existing code...
    
    double secret = 0.0;
    
    for (int i = 0; i < k; i++) {
        double li = 1.0; // Lagrange basis polynomial
        
        for (int j = 0; j < k; j++) {
            if (i != j) {
                li *= (double)(0 - points[j].x) / (double)(points[i].x - points[j].x);
            }
        }
        
        secret += points[i].y * li;
    }
    
    // ...existing code...
    return (long long)round(secret);
}

// Read JSON from stdin
char* read_json_input() {
    char* buffer = malloc(MAX_JSON_SIZE);
    if (!buffer) return NULL;
    
    buffer[0] = '\0';
    printf("Please enter the JSON input (or paste it):\n");
    printf("Enter 'END' on a new line when finished:\n\n");
    
    char line[1000];
    int total_length = 0;
    
    while (fgets(line, sizeof(line), stdin)) {
        // Check for end marker
        if (strncmp(line, "END", 3) == 0) break;
        
        // Add line to buffer
        int line_len = strlen(line);
        if (total_length + line_len < MAX_JSON_SIZE - 1) {
            strcat(buffer, line);
            total_length += line_len;
        }
    }
    
    return buffer;
}

// Alternative: Read entire JSON as single input
char* read_json_single_input() {
    char* buffer = malloc(MAX_JSON_SIZE);
    if (!buffer) return NULL;
    
    printf("Paste your complete JSON input and press Enter:\n");
    
    if (fgets(buffer, MAX_JSON_SIZE, stdin)) {
        return buffer;
    }
    
    free(buffer);
    return NULL;
}

int main() {
    printf("HASHIRA PLACEMENTS ASSIGNMENT\n");
    printf("Shamir's Secret Sharing Solver\n");
    printf("===============================\n\n");
    
    // Option 1: Read multiline JSON
    char* json_input = read_json_input();
    
    if (!json_input || strlen(json_input) < 10) {
        printf("Invalid or empty input. Trying single line input...\n");
        free(json_input);
        
        // Option 2: Read single line JSON
        json_input = read_json_single_input();
    }
    
    if (!json_input || strlen(json_input) < 10) {
        printf("Error: No valid JSON input provided!\n");
        printf("Please provide JSON input in the format specified.\n");
        return 1;
    }
    
    // ...existing code...
    
    SecretSharingData data;
    if (!parse_json_input(json_input, &data)) {
        printf("Error: Failed to parse JSON or insufficient shares!\n");
        printf("Need at least %d shares, found %d\n", data.k, data.share_count);
        free(json_input);
        return 1;
    }
    
    // Calculate the secret
    long long secret = lagrange_interpolation(data.shares, data.k);
    
    printf("\n" "================================\n");
    printf("🎉 SECRET FOUND: %lld\n", secret);
    printf("================================\n");
    
    free(json_input);
    return 0;
}
