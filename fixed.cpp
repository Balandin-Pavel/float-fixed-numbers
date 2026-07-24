#include <iostream>
#include <cstdio>
#include <cstdint>
#include <cstdlib>

void print_fixed(uint64_t int_part, uint64_t milli_part, bool is_negative) {
    if (int_part == 0 && milli_part == 0) {
        is_negative = false;
    }
    printf("%s%llu.%03llu\n", is_negative ? "-" : "", (unsigned long long)int_part, (unsigned long long)milli_part);
}

inline void get_parts(int64_t val, int b, uint64_t &int_part, uint64_t &milli_part, uint64_t &rem, uint64_t &half_den) {
    uint64_t abs_val = (val < 0) ? -(uint64_t)val : (uint64_t)val;
    uint64_t den = (uint64_t)1 << b;
    
    int_part = abs_val >> b;
    uint64_t frac_bits = abs_val & (den - 1);
    uint64_t num = frac_bits * 1000;
    
    milli_part = num >> b;
    rem = num & (den - 1);
    half_den = den >> 1;
}

inline void apply_carry(uint64_t &int_part, uint64_t &milli_part) {
    if (milli_part >= 1000) {
        milli_part -= 1000;
        int_part++;
    }
}

inline int64_t sign_extend(uint64_t value, int total_bits) {
    if (total_bits >= 64) return (int64_t)value;
    int shift = 64 - total_bits;
    return ((int64_t)value << shift) >> shift;
}

void round_toward_zero(int64_t val, int b) {
    uint64_t int_part, milli_part, rem, half_den;
    get_parts(val, b, int_part, milli_part, rem, half_den);
    print_fixed(int_part, milli_part, val < 0);
}

void round_toward_nearest_even(int64_t val, int b) {
    uint64_t int_part, milli_part, rem, half_den;
    get_parts(val, b, int_part, milli_part, rem, half_den);

    if (rem > half_den || (rem == half_den && (milli_part & 1))) {
        milli_part++;
        apply_carry(int_part, milli_part);
    }
    print_fixed(int_part, milli_part, val < 0);
}

void round_toward_pos_inf(int64_t val, int b) {
    uint64_t int_part, milli_part, rem, half_den;
    get_parts(val, b, int_part, milli_part, rem, half_den);

    if (val >= 0 && rem > 0) {
        milli_part++;
        apply_carry(int_part, milli_part);
    }
    print_fixed(int_part, milli_part, val < 0);
}

void round_toward_neg_inf(int64_t val, int a, int b) {
    uint64_t int_part, milli_part, rem, half_den;
    get_parts(val, b, int_part, milli_part, rem, half_den);

    if (val < 0 && rem > 0) {
        milli_part++;
        apply_carry(int_part, milli_part);
    }
    print_fixed(int_part, milli_part, val < 0);
}

int64_t round_for_mul_and_div(int64_t n, int64_t d, int mode) {
    int64_t q = n / d;
    int64_t r = n % d;
    if (r == 0) return q;

    bool same_sign = (n < 0) == (d < 0);
    int64_t correction = same_sign ? 1 : -1;

    switch (mode) {
        case 0: return q;
        case 1: {
            uint64_t abs_r = (r < 0) ? -r : r;
            uint64_t abs_d = (d < 0) ? -d : d;
            if ((abs_r << 1) > abs_d || ((abs_r << 1) == abs_d && (q & 1))) {
                return q + correction;
            }
            return q;
        }
        case 2: return same_sign ? q + 1 : q;
        case 3: return same_sign ? q : q - 1;
    }
    return q;
}

void do_rounding(int64_t val, int a, int b, int rounding) {
    switch (rounding) {
        case 0: round_toward_zero(val, b); break;
        case 1: round_toward_nearest_even(val, b); break;
        case 2: round_toward_pos_inf(val, b); break;
        case 3: round_toward_neg_inf(val, a, b); break;
    }
}

void fixed_add(uint32_t number1, uint32_t number2, int a, int b, int rounding) {
    int64_t res = sign_extend(number1 + number2, a + b);
    do_rounding(res, a, b, rounding);
}

void fixed_sub(uint32_t number1, uint32_t number2, int a, int b, int rounding) {
    int64_t res = sign_extend(number1 - number2, a + b);
    do_rounding(res, a, b, rounding);
}

void fixed_mul(uint32_t number1, uint32_t number2, int a, int b, int rounding) {
    int64_t op1 = sign_extend(number1, a + b);
    int64_t op2 = sign_extend(number2, a + b);
    int64_t rounded = round_for_mul_and_div(op1 * op2, 1LL << b, rounding);
    do_rounding(sign_extend(rounded, a + b), a, b, rounding);
}

void fixed_div(uint32_t number1, uint32_t number2, int a, int b, int rounding) {
    int64_t op2 = sign_extend(number2, a + b);
    int64_t op1 = sign_extend(number1, a + b);
    int64_t rounded = round_for_mul_and_div(op1 * (1LL << b), op2, rounding);
    do_rounding(sign_extend(rounded, a + b), a, b, rounding);
}

void do_operation(uint32_t number1, uint32_t number2, char operation, int a, int b, int rounding) {
    switch (operation) {
        case '+': fixed_add(number1, number2, a, b, rounding); break;
        case '-': fixed_sub(number1, number2, a, b, rounding); break;
        case '*': fixed_mul(number1, number2, a, b, rounding); break;
        case '/': fixed_div(number1, number2, a, b, rounding); break;
    }
}

bool parse_hex(const char* str, uint32_t& number) {
    if (str == nullptr || *str == '\0') return false;
    char* endptr;
    number = (uint32_t)strtoul(str, &endptr, 16);
    return *endptr == '\0';
}

int main(int argc, char** argv) {
    if (argc < 3) return 0;
    
    int a, b;
    sscanf(argv[1], "%d.%d", &a, &b);
    int rounding = atoi(argv[2]);
    if (rounding > 3 or rounding < 0){
        std::cerr<< "Wrong rounding";
        return 1;
    }
    if(a+b>32){
        std::cerr<< "Wrond A.B";
        return 2;
    }
    if (argc < 4 or argc == 5 or argc > 6){
        std::cerr<<"Wrong amount of arguments";
        return 3;
    }
    uint32_t number1;
    uint32_t number2;
    if (argc == 4) {
        if (!parse_hex(argv[3], number1)) {
            std::cerr << "Wrong number";
            return 5;
        }
        do_rounding(sign_extend(number1, a + b), a, b, rounding);
    } else if (argc == 6) {
        if (!parse_hex(argv[4], number1)) {
            std::cerr << "Wrong number";
            return 5;
        }
        char operation = argv[3][0];
        if(operation != '+' and operation != '-' and operation != '*' and operation != '/'){
            std::cerr<<"Wrong operation";
            return 4;
        }
        if (!parse_hex(argv[5], number2)) {
            std::cerr << "Wrong number";
            return 5;
        }
        if (operation == '/' and sign_extend(number2, a + b) == 0){
            std::cout<<"div_by_zero";
            return 0; 
        }
        do_operation(number1, number2, operation, a, b, rounding);
    }
    return 0;
}