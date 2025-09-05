

#define MAX_SHARES 100
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#define MAX_VALUE_LENGTH 200
#define MAX_JSON_SIZE 50000

// We will define Share and SecretSharingData after BigInt is defined

// ---------------- Minimal Big Integer (base 1e9) ----------------
typedef struct {
    int sign;                // -1, 0, +1
    int len;                 // number of used limbs
    unsigned int d[512];     // limbs, little-endian, base 1e9
} BigInt;

#define BI_BASE 1000000000u

static void bi_zero(BigInt *a) {
    a->sign = 0; a->len = 0;
}

static void bi_normalize(BigInt *a) {
    while (a->len > 0 && a->d[a->len - 1] == 0u) a->len--;
    if (a->len == 0) a->sign = 0;
}

// Forward declarations used before definitions
static int bi_cmp_abs(const BigInt *a, const BigInt *b);
static void bi_sub_abs(BigInt *res, const BigInt *a, const BigInt *b);
static void bi_copy(BigInt *dst, const BigInt *src);

static void bi_from_ll(BigInt *a, long long v) {
    if (v == 0) { bi_zero(a); return; }
    unsigned long long u = (v < 0) ? (unsigned long long)(-(v + 1)) + 1ull : (unsigned long long)v;
    a->len = 0;
    while (u > 0ull) {
        a->d[a->len++] = (unsigned int)(u % BI_BASE);
        u /= BI_BASE;
    }
    a->sign = (v < 0) ? -1 : 1;
    bi_normalize(a);
}

static void bi_add_small(BigInt *res, const BigInt *a, unsigned long long m) {
    if (a->sign == 0) { bi_from_ll(res, (long long)m); return; }
    if (a->sign < 0) {
        // a + m = a - (-m)
        BigInt mm; bi_from_ll(&mm, (long long)m); mm.sign = 1;
        // res = a + mm
        BigInt ap = *a; ap.sign = 1;
        if (bi_cmp_abs(&ap, &mm) >= 0) {
            BigInt t; bi_sub_abs(&t, &ap, &mm); t.sign = -1; bi_copy(res, &t);
        } else {
            BigInt t; bi_sub_abs(&t, &mm, &ap); t.sign = 1; bi_copy(res, &t);
        }
        return;
    }
    // positive + small
    unsigned long long carry = m;
    BigInt t = *a;
    int i = 0;
    while (carry > 0ull) {
        if (i >= t.len) t.d[t.len++] = 0u;
        unsigned long long cur = (unsigned long long)t.d[i] + carry;
        t.d[i] = (unsigned int)(cur % BI_BASE);
        carry = cur / BI_BASE;
        i++;
    }
    t.sign = 1; bi_copy(res, &t);
}

static void bi_copy(BigInt *dst, const BigInt *src) {
    dst->sign = src->sign; dst->len = src->len;
    for (int i = 0; i < src->len; i++) dst->d[i] = src->d[i];
}

static int bi_cmp_abs(const BigInt *a, const BigInt *b) {
    if (a->len != b->len) return (a->len < b->len) ? -1 : 1;
    for (int i = a->len - 1; i >= 0; i--) {
        if (a->d[i] != b->d[i]) return (a->d[i] < b->d[i]) ? -1 : 1;
    }
    return 0;
}

static void bi_add_abs(BigInt *res, const BigInt *a, const BigInt *b) {
    unsigned long long carry = 0ull;
    int n = (a->len > b->len) ? a->len : b->len;
    for (int i = 0; i < n || carry; i++) {
        unsigned long long cur = carry;
        if (i < a->len) cur += a->d[i];
        if (i < b->len) cur += b->d[i];
        res->d[i] = (unsigned int)(cur % BI_BASE);
        carry = cur / BI_BASE;
        res->len = i + 1;
    }
}

static void bi_sub_abs(BigInt *res, const BigInt *a, const BigInt *b) { // assume |a|>=|b|
    long long carry = 0;
    res->len = a->len;
    for (int i = 0; i < a->len; i++) {
        long long cur = (long long)a->d[i] - (i < b->len ? b->d[i] : 0) - carry;
        if (cur < 0) { cur += BI_BASE; carry = 1; } else carry = 0;
        res->d[i] = (unsigned int)cur;
    }
    bi_normalize(res);
}

static void bi_add(BigInt *res, const BigInt *a, const BigInt *b) {
    if (a->sign == 0) { bi_copy(res, b); return; }
    if (b->sign == 0) { bi_copy(res, a); return; }
    if (a->sign == b->sign) {
        BigInt t = {0};
        bi_add_abs(&t, a, b);
        t.sign = a->sign;
        bi_copy(res, &t);
    } else {
        int cmp = bi_cmp_abs(a, b);
        if (cmp == 0) { bi_zero(res); return; }
        if (cmp > 0) {
            BigInt t = {0};
            bi_sub_abs(&t, a, b);
            t.sign = a->sign;
            bi_copy(res, &t);
        } else {
            BigInt t = {0};
            bi_sub_abs(&t, b, a);
            t.sign = b->sign;
            bi_copy(res, &t);
        }
    }
}

static void bi_mul_small(BigInt *res, const BigInt *a, unsigned long long m) {
    if (a->sign == 0 || m == 0ull) { bi_zero(res); return; }
    unsigned long long carry = 0ull;
    BigInt t = {0};
    for (int i = 0; i < a->len || carry; i++) {
        unsigned long long cur = carry;
        if (i < a->len) cur += (unsigned long long)a->d[i] * m;
        t.d[i] = (unsigned int)(cur % BI_BASE);
        carry = cur / BI_BASE;
        t.len = i + 1;
    }
    t.sign = a->sign;
    bi_copy(res, &t);
}

static unsigned long long bi_mod_small(const BigInt *a, unsigned long long m) {
    if (m == 0ull) return 0ull;
    unsigned long long rem = 0ull;
    for (int i = a->len - 1; i >= 0; i--) {
        unsigned long long cur = a->d[i] + rem * BI_BASE;
        rem = cur % m;
        if (i == 0) break; // avoid i>=0 with unsigned
    }
    return rem;
}

static void bi_div_small(BigInt *res, const BigInt *a, unsigned long long m, unsigned long long *out_rem) {
    BigInt t = {0};
    if (m == 0ull || a->sign == 0) { bi_zero(res); if (out_rem) *out_rem = 0ull; return; }
    unsigned long long rem = 0ull;
    for (int i = a->len - 1; i >= 0; i--) {
        unsigned long long cur = a->d[i] + rem * BI_BASE;
        unsigned long long q = cur / m;
        rem = cur % m;
        t.d[i] = (unsigned int)q;
        if (i == 0) break;
    }
    t.len = a->len; t.sign = a->sign;
    bi_normalize(&t);
    bi_copy(res, &t);
    if (out_rem) *out_rem = rem;
}

static void bi_print_dec(const BigInt *a) {
    if (a->sign == 0) { printf("0"); return; }
    BigInt tmp; bi_copy(&tmp, a);
    if (tmp.sign < 0) printf("-"); tmp.sign = 1;
    // collect chunks in base 1e9
    unsigned int chunks[1024]; int cnt = 0;
    while (tmp.len > 0) {
        unsigned long long rem = 0ull; BigInt q;
        bi_div_small(&q, &tmp, BI_BASE, &rem);
        chunks[cnt++] = (unsigned int)rem;
        bi_copy(&tmp, &q);
    }
    // print most significant chunk without padding
    printf("%u", chunks[cnt - 1]);
    for (int i = cnt - 2; i >= 0; i--) printf("%09u", chunks[i]);
}

static unsigned long long ull_gcd(unsigned long long a, unsigned long long b) {
    while (b) { unsigned long long r = a % b; a = b; b = r; }
    return a;
}

static unsigned long long ull_lcm(unsigned long long a, unsigned long long b) {
    if (a == 0 || b == 0) return 0;
    return (a / ull_gcd(a, b)) * b;
}

// Now that BigInt is defined, redefine Share to include BigInt y
typedef struct {
    int x;
    BigInt y;
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

// Convert string from given base to BigInt
void base_to_bigint(const char* value_str, int base, BigInt *out) {
    bi_zero(out);
    if (base < 2 || base > 36) return;
    for (const char *p = value_str; *p; ++p) {
        int d = char_to_digit(*p);
        if (d < 0 || d >= base) return;
        BigInt t; bi_mul_small(&t, out, (unsigned long long)base);
        BigInt t2; bi_add_small(&t2, &t, (unsigned long long)d);
        bi_copy(out, &t2);
    }
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
    // Parse each share dynamically
    char* current_pos = (char*)json_str;
    for (int share_id = 1; share_id <= 100; share_id++) {
        char search_pattern[20];
        sprintf(search_pattern, "\"%d\":", share_id);
        char* share_block = strstr(current_pos, search_pattern);
        if (!share_block) continue;
        // Find opening brace of this share object
        char *open_brace = strchr(share_block, '{');
        if (!open_brace) continue;
        char* block_end = open_brace + 1;
        int depth = 1;
        while (*block_end && depth > 0) {
            if (*block_end == '{') depth++;
            else if (*block_end == '}') depth--;
            block_end++;
        }
        if (depth != 0) continue; // malformed
        char* base_line = strstr(open_brace, "\"base\"");
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
        char* value_line = strstr(open_brace, "\"value\"");
        if (!value_line || value_line > block_end) continue;
        char value_str[MAX_VALUE_LENGTH] = {0};
        extract_string_after_colon(value_line, value_str);
        data->shares[data->share_count].x = share_id;
        base_to_bigint(value_str, base, &data->shares[data->share_count].y);
        data->share_count++;
    }
    return (data->share_count >= data->k) ? 1 : 0;
}

// Compute secret f(0) for a subset using rational Lagrange with common denominator
typedef struct { BigInt num; unsigned long long den; } BigFrac;

static void compute_secret_fraction_for_subset(Share subset[], int k, BigFrac *out) {
    // Prepare denominators and numerators per term
    long long term_num[64];
    unsigned long long term_den[64];
    int term_sign[64];
    for (int i = 0; i < k; i++) {
        long long num = 1;
        long long den = 1;
        for (int j = 0; j < k; j++) if (j != i) {
            num *= -(subset[j].x);
            den *= (subset[i].x - subset[j].x);
        }
        long long nsign = 1;
        if (num < 0) { nsign = -nsign; num = -num; }
        if (den < 0) { nsign = -nsign; den = -den; }
        term_num[i] = num;
        term_den[i] = (unsigned long long)den;
        term_sign[i] = (int)nsign;
    }
    // Common denominator
    unsigned long long D = 1ull;
    for (int i = 0; i < k; i++) D = ull_lcm(D, term_den[i]);
    // Sum aligned numerators
    BigInt sum; bi_zero(&sum);
    for (int i = 0; i < k; i++) {
        unsigned long long factor = D / term_den[i];
        BigInt yi; bi_copy(&yi, &subset[i].y);
        BigInt t; bi_mul_small(&t, &yi, (unsigned long long)term_num[i]);
        if (factor != 1ull) { BigInt t2; bi_mul_small(&t2, &t, factor); bi_copy(&t, &t2); }
        t.sign = (t.sign == 0) ? 0 : (t.sign * term_sign[i]);
        BigInt newSum; bi_add(&newSum, &sum, &t); bi_copy(&sum, &newSum);
    }
    // Reduce fraction sum/D
    BigInt numAbs = sum; if (numAbs.sign < 0) numAbs.sign = 1;
    unsigned long long r = bi_mod_small(&numAbs, D);
    unsigned long long g = ull_gcd(D, r);
    if (g > 1ull) { D /= g; BigInt q; unsigned long long rem; bi_div_small(&q, &sum, g, &rem); bi_copy(&sum, &q); }
    out->num = sum; out->den = D;
}

// Detect corrupted shares using exact fraction equality across all k-combinations
void detect_and_report_corrupted_shares(Share points[], int n, int k) {
    // Dynamic arrays to avoid large stack allocations
    int cap = 256;
    BigFrac *secrets = (BigFrac*)malloc(sizeof(BigFrac) * cap);
    unsigned long long *masks = (unsigned long long*)malloc(sizeof(unsigned long long) * cap);
    int comb_count = 0;

    // Generate all k-combinations
    int idx[MAX_SHARES];
    for (int i = 0; i < k; i++) idx[i] = 0 + i;
    int has_more = (n >= k);
    while (has_more) {
        // Build subset and mask
        Share subset[MAX_SHARES];
        unsigned long long mask = 0ull;
        for (int i = 0; i < k; i++) { subset[i] = points[idx[i]]; mask |= (1ull << idx[i]); }

        if (comb_count >= cap) {
            cap *= 2;
            secrets = (BigFrac*)realloc(secrets, sizeof(BigFrac) * cap);
            masks = (unsigned long long*)realloc(masks, sizeof(unsigned long long) * cap);
        }
        compute_secret_fraction_for_subset(subset, k, &secrets[comb_count]);
        masks[comb_count] = mask;
        comb_count++;

        // Next combination
        int j;
        for (j = k - 1; j >= 0; j--) {
            if (idx[j] < n - k + j) { idx[j]++; for (int l = j + 1; l < k; l++) idx[l] = idx[l - 1] + 1; break; }
        }
        if (j < 0) has_more = 0;
    }

    // Find consensus (max frequency of equal fractions)
    int best = 0, bestCount = 1;
    for (int i = 0; i < comb_count; i++) {
        int cnt = 1;
        for (int j = i + 1; j < comb_count; j++) {
            if (secrets[i].den == secrets[j].den && secrets[i].num.sign == secrets[j].num.sign &&
                bi_cmp_abs(&secrets[i].num, &secrets[j].num) == 0) cnt++;
        }
        if (cnt > bestCount) { bestCount = cnt; best = i; }
    }
    BigFrac consensus = secrets[best];

    // Determine corrupted shares: any share not present in a consensus combination
    int corrupted[MAX_SHARES] = {0};
    for (int i = 0; i < n; i++) {
        int ok = 0;
        for (int c = 0; c < comb_count; c++) {
            if (secrets[c].den == consensus.den && secrets[c].num.sign == consensus.num.sign &&
                bi_cmp_abs(&secrets[c].num, &consensus.num) == 0) {
                if (masks[c] & (1ull << i)) { ok = 1; break; }
            }
        }
        corrupted[i] = !ok;
    }

    // Output secret
    if (consensus.den == 1ull) {
        bi_print_dec(&consensus.num); printf("\n");
    } else {
        bi_print_dec(&consensus.num); printf("/"); printf("%llu\n", consensus.den);
    }
    // Output corrupted indices
    printf("corrupted_indices: [");
    int first = 1;
    for (int i = 0; i < n; i++) if (corrupted[i]) { if (!first) printf(", "); printf("%d", points[i].x); first = 0; }
    printf("]\n");

    free(secrets);
    free(masks);
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
    
    detect_and_report_corrupted_shares(data.shares, data.share_count, data.k);
    free(json_input);
    return 0;
}
