#include <algorithm>
#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

const int ZERO_EXP_SENTINEL = -1000000;

struct My_float{
    uint32_t mant;
    uint32_t exp;
    uint32_t sign;
    uint32_t mant_bits;
    uint32_t mant_max;
    uint32_t exp_bits;
    uint32_t exp_max;
    uint32_t bias;
    uint32_t start_number;
};

enum FloatClass{
    ZERO,
    DENORMALIZED,
    NORMALIZED,
    INFINITY_VAL,
    QUIET_NAN,
    SIGNALING_NAN
};

struct RoundingInfo{
    bool guard;
    bool round;
    bool sticky;
};

bool is_nan_class(FloatClass c){
    return c == QUIET_NAN || c == SIGNALING_NAN;
}

uint64_t low_mask64(int bits){
    if (bits <= 0){
        return 0;
    }
    if (bits >= 64){
        return UINT64_MAX;
    }
    return (1ull << bits) - 1ull;
}

void shift_right_sticky(uint64_t& value, int shift, bool& sticky){
    if (shift <= 0){
        return;
    }
    if (shift >= 64){
        sticky = sticky || (value != 0);
        value = 0;
        return;
    }
    sticky = sticky || ((value & low_mask64(shift)) != 0);
    value >>= shift;
}

RoundingInfo extract_rounding_info(uint64_t value, int shift, bool sticky){
    RoundingInfo info{};
    info.guard = false;
    info.round = false;
    info.sticky = sticky;

    if (shift <= 0){
        return info;
    }
    if (shift >= 64){
        info.sticky = info.sticky || (value != 0);
        return info;
    }

    info.guard = ((value >> (shift - 1)) & 1ull) != 0;
    if (shift >= 2){
        info.round = ((value >> (shift - 2)) & 1ull) != 0;
    }
    if (shift > 2){
        info.sticky = info.sticky || ((value & low_mask64(shift - 2)) != 0);
    }
    return info;
}

bool should_round_up(uint32_t sign, RoundingInfo info, uint64_t mant, int rounding){
    bool has_extra = info.guard || info.round || info.sticky;
    switch (rounding){
        case 0: return false;
        case 1:
            if (!info.guard){
                return false;
            }
            if (info.round || info.sticky){
                return true;
            }
            return (mant & 1ull) != 0;
        case 2: return (sign == 0) && has_extra;
        case 3: return (sign == 1) && has_extra;
        default: return false;
    }
}

void set_format(My_float& number, const std::string& fmt){
    if (fmt == "h"){
        number.mant_bits = 10;
        number.exp_bits = 5;
        number.bias = 15;
    }else if (fmt == "s"){
        number.mant_bits = 23;
        number.exp_bits = 8;
        number.bias = 127;
    }else if (fmt == "e5m2"){
        number.mant_bits = 2;
        number.exp_bits = 5;
        number.bias = 15;
    }else if (fmt == "e4m3fn"){
        number.mant_bits = 3;
        number.exp_bits = 4;
        number.bias = 7;
    }else if (fmt == "e2m1"){
        number.mant_bits = 1;
        number.exp_bits = 2;
        number.bias = 1;
    }
    number.mant_max = (1u << number.mant_bits) - 1u;
    number.exp_max = (1u << number.exp_bits) - 1u;
}

My_float do_fmt(uint32_t value, const std::string& fmt){
    My_float number{};
    set_format(number, fmt);
    uint32_t total_bits = number.mant_bits + number.exp_bits + 1u;
    uint32_t mask = (total_bits >= 32) ? 0xFFFFFFFFu : ((1u << total_bits) - 1u);

    number.sign = (value >> (number.mant_bits + number.exp_bits)) & 1u;
    number.exp = (value >> number.mant_bits) & number.exp_max;
    number.mant = value & number.mant_max;
    number.start_number = value & mask;

    return number;
}

void copy_format(My_float& dest, const My_float& src){
    dest.mant_bits = src.mant_bits;
    dest.mant_max = src.mant_max;
    dest.exp_bits = src.exp_bits;
    dest.exp_max = src.exp_max;
    dest.bias = src.bias;
}

void pack_result(My_float& number){
    number.start_number = (number.sign << (number.mant_bits + number.exp_bits)) | (number.exp << number.mant_bits) | number.mant;
}

FloatClass classify(const My_float& number, const std::string& fmt = ""){
    bool exp_zero = (number.exp == 0);
    bool exp_max = (number.exp == number.exp_max);
    bool mant_zero = (number.mant == 0);

    if (fmt == "e4m3fn"){
        if (exp_zero) return mant_zero ? ZERO : DENORMALIZED;
        if (number.exp == 15 && number.mant == 7) return QUIET_NAN;
        return NORMALIZED;
    }
    if (fmt == "e2m1"){
        if (exp_zero) return mant_zero ? ZERO : DENORMALIZED;
        return NORMALIZED;
    }
    if (exp_zero) return mant_zero ? ZERO : DENORMALIZED;

    if (exp_max){
        if (mant_zero) return INFINITY_VAL;
        return (number.mant & (1u << (number.mant_bits - 1))) ? QUIET_NAN : SIGNALING_NAN;
    }
    return NORMALIZED;
}

My_float make_special(const My_float& ref, uint32_t sign, uint32_t exp, uint32_t mant){
    My_float result = ref;
    result.sign = sign;
    result.exp = exp;
    result.mant = mant;
    pack_result(result);
    return result;
}

My_float make_zero(const My_float& ref, uint32_t sign){
    return make_special(ref, sign, 0, 0);
}

My_float make_inf(const My_float& ref, uint32_t sign, const std::string& fmt = ""){
    if (fmt == "e4m3fn") return make_special(ref, sign, 15, 6);
    if (fmt == "e2m1") return make_special(ref, sign, ref.exp_max, ref.mant_max);
    return make_special(ref, sign, ref.exp_max, 0);
}

My_float make_max(const My_float& ref, uint32_t sign, const std::string& fmt = ""){
    return make_special(ref, sign, ref.exp_max - 1, ref.mant_max);
}

My_float make_nan(const My_float& ref, const std::string& fmt = ""){
    if (fmt == "e4m3fn") return make_special(ref, 1, 15, 7);
    if (fmt == "e2m1") return make_special(ref, 0, 0, 0);
    uint32_t nan_mant = 0;
    if (ref.mant_bits > 0){
        nan_mant = 1u << (ref.mant_bits - 1);
    }
    return make_special(ref, 1, ref.exp_max, nan_mant);
}

My_float propagate_nan(const My_float& number, const std::string& fmt = ""){
    My_float result = number;

    if (fmt == "e4m3fn"){
        result.exp = 15;
        result.mant = 7;
    }else if (fmt != "e2m1" && result.mant_bits > 0){
        result.mant |= (1u << (result.mant_bits - 1));
    }
    pack_result(result);
    return result;
}

My_float make_overflow_result(const My_float& ref, uint32_t sign, int rounding, const std::string& fmt){
    if (rounding == 0) return make_max(ref, sign, fmt);
    if (rounding == 2 && sign == 1) return make_max(ref, sign, fmt);
    if (rounding == 3 && sign == 0) return make_max(ref, sign, fmt);
    return make_inf(ref, sign, fmt);
}

int get_hex_width(const std::string& fmt){
    if (fmt == "h") return 4;
    if (fmt == "s") return 8;
    if (fmt == "e5m2" || fmt == "e4m3fn") return 2;
    if (fmt == "e2m1") return 1;
    return 4;
}

std::string format_float(const My_float& number, const std::string& fmt){
    char buffer[256];
    FloatClass fc = classify(number, fmt);
    int hex_width = get_hex_width(fmt);
    int mant_hex_width = (number.mant_bits + 3) / 4;

    if (mant_hex_width == 0){
        mant_hex_width = 1;
    }

    if (fc == QUIET_NAN || fc == SIGNALING_NAN){
        std::snprintf(buffer, sizeof(buffer), "nan 0x%0*X", hex_width, number.start_number);
        return buffer;
    }

    if (fc == INFINITY_VAL){
        std::snprintf(buffer, sizeof(buffer), "%sinf 0x%0*X", number.sign ? "-" : "", hex_width, number.start_number);
        return buffer;
    }

    if (fc == ZERO){
        std::snprintf(buffer, sizeof(buffer), "%s0x0.%0*xp+0 0x%0*X", number.sign ? "-" : "", mant_hex_width, 0, hex_width, number.start_number);
        return buffer;
    }

    if (fc == DENORMALIZED){
        int leading_pos = -1;
        for (int i = static_cast<int>(number.mant_bits) - 1; i >= 0; i--){
            if ((number.mant & (1u << i)) != 0){
                leading_pos = i;
                break;
            }
        }

        if (leading_pos >= 0){
            int shift = static_cast<int>(number.mant_bits) - 1 - leading_pos;
            uint32_t norm_mant = (number.mant << (shift + 1)) & number.mant_max;
            int exp_val = (1 - static_cast<int>(number.bias)) - (shift + 1);
            uint32_t shift_amount = mant_hex_width * 4 - number.mant_bits;
            uint32_t mant_shifted = norm_mant << shift_amount;

            std::snprintf(buffer, sizeof(buffer), "%s0x1.%0*xp%+d 0x%0*X", number.sign ? "-" : "", mant_hex_width, mant_shifted, exp_val, hex_width, number.start_number);
            return buffer;
        }
    }

    uint32_t shift_amount = mant_hex_width * 4 - number.mant_bits;
    uint32_t mant_shifted = number.mant << shift_amount;
    int exp_val = static_cast<int>(number.exp) - static_cast<int>(number.bias);
    std::snprintf(buffer, sizeof(buffer), "%s0x1.%0*xp%+d 0x%0*X", number.sign ? "-" : "", mant_hex_width, mant_shifted, exp_val, hex_width, number.start_number);
    return buffer;
}

void print_float(const My_float& number, const std::string& fmt){
    std::cout << format_float(number, fmt) << '\n';
}

std::string capture_output(const My_float& number, const std::string& fmt){
    return format_float(number, fmt);
}

void get_full_mant_exp(const My_float& number, uint64_t& mant, int& exp_val, const std::string& fmt = ""){
    FloatClass fc = classify(number, fmt);
    uint64_t hidden = 1ull << number.mant_bits;

    if (fc == ZERO){
        mant = 0;
        exp_val = ZERO_EXP_SENTINEL;
        return;
    }
    if (fc == DENORMALIZED){
        mant = number.mant;
        exp_val = 1 - static_cast<int>(number.bias);
        while (mant != 0 && mant < hidden){
            mant <<= 1;
            exp_val--;
        }
        return;
    }
    mant = number.mant | hidden;
    exp_val = static_cast<int>(number.exp) - static_cast<int>(number.bias);
}

bool handle_special_add(const My_float& a, const My_float& b, My_float& result, const std::string& fmt){
    FloatClass ca = classify(a, fmt);
    FloatClass cb = classify(b, fmt);

    copy_format(result, a);
    if (is_nan_class(ca)){
        result = propagate_nan(a, fmt);
        return true;
    }
    if (is_nan_class(cb)){
        result = propagate_nan(b, fmt);
        return true;
    }
    if (ca == INFINITY_VAL && cb == INFINITY_VAL){
        result = (a.sign != b.sign) ? make_nan(a, fmt) : a;
        return true;
    }
    if (ca == INFINITY_VAL){
        result = a;
        return true;
    }
    if (cb == INFINITY_VAL){
        result = b;
        return true;
    }
    if (ca == ZERO && cb == ZERO){
        result = make_zero(a, (a.sign == b.sign) ? a.sign : 0);
        return true;
    }
    if (ca == ZERO){
        result = b;
        return true;
    }
    if (cb == ZERO){
        result = a;
        return true;
    }
    return false;
}

bool handle_special_mul(const My_float& a, const My_float& b, My_float& result, const std::string& fmt){
    FloatClass ca = classify(a, fmt);
    FloatClass cb = classify(b, fmt);
    uint32_t sign = a.sign ^ b.sign;

    copy_format(result, a);
    if (is_nan_class(ca)){
        result = propagate_nan(a, fmt);
        return true;
    }
    if (is_nan_class(cb)){
        result = propagate_nan(b, fmt);
        return true;
    }
    if ((ca == INFINITY_VAL && cb == ZERO) || (ca == ZERO && cb == INFINITY_VAL)){
        result = make_nan(a, fmt);
        return true;
    }
    if (ca == INFINITY_VAL || cb == INFINITY_VAL){
        result = make_inf(a, sign, fmt);
        return true;
    }
    if (ca == ZERO || cb == ZERO){
        result = make_zero(a, sign);
        return true;
    }
    return false;
}

bool handle_special_div(const My_float& a, const My_float& b, My_float& result, const std::string& fmt){
    FloatClass ca = classify(a, fmt);
    FloatClass cb = classify(b, fmt);
    uint32_t sign = a.sign ^ b.sign;

    copy_format(result, a);
    if (is_nan_class(ca)){
        result = propagate_nan(a, fmt);
        return true;
    }
    if (is_nan_class(cb)){
        result = propagate_nan(b, fmt);
        return true;
    }
    if ((ca == INFINITY_VAL && cb == INFINITY_VAL) || (ca == ZERO && cb == ZERO)){
        result = make_nan(a, fmt);
        return true;
    }
    if (cb == ZERO){
        result = make_inf(a, sign, fmt);
        return true;
    }
    if (ca == INFINITY_VAL){
        result = make_inf(a, sign, fmt);
        return true;
    }
    if (cb == INFINITY_VAL){
        result = make_zero(a, sign);
        return true;
    }
    if (ca == ZERO){
        result = make_zero(a, sign);
        return true;
    }
    return false;
}

bool handle_special_fma(const My_float& a, const My_float& b, const My_float& c, My_float& result, int rounding, const std::string& fmt){
    FloatClass ca = classify(a, fmt);
    FloatClass cb = classify(b, fmt);
    FloatClass cc = classify(c, fmt);
    uint32_t prod_sign = a.sign ^ b.sign;
    copy_format(result, a);
    if (is_nan_class(ca)){
        result = propagate_nan(a, fmt);
        return true;
    }

    if (is_nan_class(cb)){
        result = propagate_nan(b, fmt);
        return true;
    }
    if (is_nan_class(cc)){
        result = propagate_nan(c, fmt);
        return true;
    }
    if ((ca == INFINITY_VAL && cb == ZERO) || (ca == ZERO && cb == INFINITY_VAL)){
        result = make_nan(a, fmt);
        return true;
    }
    if ((ca == INFINITY_VAL || cb == INFINITY_VAL) && cc == INFINITY_VAL){
        result = (prod_sign != c.sign) ? make_nan(a, fmt) : make_inf(a, prod_sign, fmt);
        return true;
    }
    if (ca == INFINITY_VAL || cb == INFINITY_VAL){
        result = make_inf(a, prod_sign, fmt);
        return true;
    }
    if (cc == INFINITY_VAL){
        result = c;
        return true;
    }
    if (ca == ZERO || cb == ZERO){
        if (cc == ZERO){
            result = make_zero(a, (prod_sign == c.sign) ? prod_sign : ((rounding == 3) ? 1u : 0u));
        }else{
            result = c;
        }
        return true;
    }
    return false;
}

My_float float_add(const My_float& a, const My_float& b, int rounding, const std::string& fmt){
    My_float result{};
    if (handle_special_add(a, b, result, fmt)){
        if (classify(result, fmt) == ZERO && rounding == 3){
            result.sign = 1;
            pack_result(result);
        }
        return result;
    }
    copy_format(result, a);
    uint64_t mant1 = 0;
    uint64_t mant2 = 0;
    int exp1 = 0;
    int exp2 = 0;

    get_full_mant_exp(a, mant1, exp1, fmt);
    get_full_mant_exp(b, mant2, exp2, fmt);

    const int guard_bits = 3;
    mant1 <<= guard_bits;
    mant2 <<= guard_bits;
    int exp_result = std::max(exp1, exp2);
    bool sticky1 = false;
    bool sticky2 = false;

    if (exp1 < exp_result){
        shift_right_sticky(mant1, exp_result - exp1, sticky1);
    }

    if (exp2 < exp_result){
        shift_right_sticky(mant2, exp_result - exp2, sticky2);
    }
    uint64_t mant_result = 0;
    bool sticky_result = false;
    if (a.sign == b.sign){
        result.sign = a.sign;
        mant_result = mant1 + mant2;
        sticky_result = sticky1 || sticky2;
    }else{
        bool mant1_greater = (mant1 > mant2) || (mant1 == mant2 && sticky1 > sticky2);
        bool mant2_greater = (mant2 > mant1) || (mant1 == mant2 && sticky2 > sticky1);

        if (mant1_greater){
            result.sign = a.sign;
            mant_result = mant1 - mant2;

            if (sticky2 && !sticky1){
                mant_result--;
                sticky_result = true;
            }else if (sticky1 && sticky2){
                sticky_result = true;
            }else{
                sticky_result = sticky1;
            }
        }else if (mant2_greater){
            result.sign = b.sign;
            mant_result = mant2 - mant1;

            if (sticky1 && !sticky2){
                mant_result--;
                sticky_result = true;
            }else if (sticky1 && sticky2){
                sticky_result = true;
            }else{
                sticky_result = sticky2;
            }
        }else{
            result.sign = (rounding == 3) ? 1u : 0u;
            return make_zero(result, result.sign);
        }
    }if (mant_result == 0 && !sticky_result) return make_zero(result, (rounding == 3) ? 1u : 0u);

    uint64_t hidden = 1ull << (result.mant_bits + guard_bits);
    int min_exp = 1 - static_cast<int>(result.bias) - static_cast<int>(result.mant_bits);

    while (mant_result >= (hidden << 1)){
        sticky_result = sticky_result || ((mant_result & 1ull) != 0);
        mant_result >>= 1;
        exp_result++;
    }

    while (mant_result != 0 && mant_result < hidden && exp_result > min_exp){
        mant_result <<= 1;
        exp_result--;
    }

    RoundingInfo info{};
    info.guard = ((mant_result >> 2) & 1ull) != 0;
    info.round = ((mant_result >> 1) & 1ull) != 0;
    info.sticky = ((mant_result & 1ull) != 0) || sticky_result;

    mant_result >>= guard_bits;
    if (should_round_up(result.sign, info, mant_result, rounding)){
        mant_result++;

        if (mant_result >= (1ull << (result.mant_bits + 1))){
            mant_result >>= 1;
            exp_result++;
        }
    }

    int biased_exp = exp_result + static_cast<int>(result.bias);
    if (biased_exp >= static_cast<int>(result.exp_max)) return make_overflow_result(result, result.sign, rounding, fmt);
    if (biased_exp <= 0){
        int shift = 1 - biased_exp;
        if (shift < 64){
            RoundingInfo denorm{};
            denorm.guard = (shift >= 1) ? (((mant_result >> (shift - 1)) & 1ull) != 0) : false;
            denorm.round = (shift >= 2) ? (((mant_result >> (shift - 2)) & 1ull) != 0) : false;
            denorm.sticky = (shift > 2) ? ((mant_result & low_mask64(shift - 2)) != 0) : false;

            mant_result >>= shift;

            if (should_round_up(result.sign, denorm, mant_result, rounding)){
                mant_result++;
                if (mant_result >= (1ull << result.mant_bits)){
                    biased_exp = 1;
                    mant_result &= result.mant_max;
                }
            }
        }else{
            mant_result = 0;
        }
        if (mant_result == 0 && biased_exp <= 0){
            return make_zero(result, result.sign);
        }
        if (biased_exp <= 0){
            biased_exp = 0;
        }
    }else{
        mant_result &= result.mant_max;
    }
    result.exp = static_cast<uint32_t>(biased_exp);
    result.mant = static_cast<uint32_t>(mant_result & result.mant_max);
    pack_result(result);
    return result;
}

My_float float_sub(const My_float& a, const My_float& b, int rounding, const std::string& fmt){
    FloatClass ca = classify(a, fmt);
    FloatClass cb = classify(b, fmt);

    if (is_nan_class(ca)) return propagate_nan(a, fmt);
    if (is_nan_class(cb)) return propagate_nan(b, fmt);

    My_float neg_b = b;
    neg_b.sign ^= 1u;
    pack_result(neg_b);
    return float_add(a, neg_b, rounding, fmt);
}

My_float float_mul(const My_float& a, const My_float& b, int rounding, const std::string& fmt){
    My_float result{};
    if (handle_special_mul(a, b, result, fmt)) return result;
    copy_format(result, a);
    result.sign = a.sign ^ b.sign;
    uint64_t mant1 = 0;
    uint64_t mant2 = 0;
    int exp1 = 0;
    int exp2 = 0;

    get_full_mant_exp(a, mant1, exp1, fmt);
    get_full_mant_exp(b, mant2, exp2, fmt);

    uint64_t mant_prod = mant1 * mant2;
    int exp_result = exp1 + exp2;

    uint64_t hidden = 1ull << result.mant_bits;
    uint64_t double_hidden = 1ull << (2 * result.mant_bits);
    bool sticky = false;

    while (mant_prod >= (double_hidden << 1)){
        sticky = sticky || ((mant_prod & 1ull) != 0);
        mant_prod >>= 1;
        exp_result++;
    }
    while (mant_prod != 0 && mant_prod < double_hidden){
        mant_prod <<= 1;
        exp_result--;
    }

    int biased_exp = exp_result + static_cast<int>(result.bias);

    if (biased_exp <= 0){
        int total_shift = static_cast<int>(result.mant_bits) + 1 - biased_exp;
        RoundingInfo info{};

        if (total_shift < 64){
            info = extract_rounding_info(mant_prod, total_shift, sticky);
            mant_prod >>= total_shift;
        }else{
            info.sticky = (mant_prod != 0) || sticky;
            mant_prod = 0;
        }
        if (should_round_up(result.sign, info, mant_prod, rounding)){
            mant_prod++;

            if (mant_prod >= hidden){
                biased_exp = 1;
                mant_prod &= result.mant_max;
            }
        }

        if (mant_prod == 0 && biased_exp <= 0) return make_zero(result, result.sign);

        if (biased_exp <= 0){
            biased_exp = 0;
        }

        result.exp = static_cast<uint32_t>(biased_exp);
        result.mant = static_cast<uint32_t>(mant_prod & result.mant_max);
        pack_result(result);
        return result;
    }

    int shift = static_cast<int>(result.mant_bits);
    RoundingInfo info = extract_rounding_info(mant_prod, shift, sticky);
    mant_prod >>= shift;

    if (mant_prod >= (hidden << 1)){
        info.sticky = info.sticky || info.round;
        info.round = info.guard;
        info.guard = (mant_prod & 1ull) != 0;
        mant_prod >>= 1;
        exp_result++;
        biased_exp++;
    }

    if (should_round_up(result.sign, info, mant_prod, rounding)){
        mant_prod++;
        if (mant_prod >= (hidden << 1)){
            mant_prod >>= 1;
            biased_exp++;
        }
    }

    if (biased_exp >= static_cast<int>(result.exp_max)) return make_overflow_result(result, result.sign, rounding, fmt);

    result.exp = static_cast<uint32_t>(biased_exp);
    result.mant = static_cast<uint32_t>(mant_prod & result.mant_max);
    pack_result(result);
    return result;
}

My_float float_div(const My_float& a, const My_float& b, int rounding, const std::string& fmt){
    My_float result{};
    if (handle_special_div(a, b, result, fmt)) return result;
    copy_format(result, a);
    result.sign = a.sign ^ b.sign;

    uint64_t mant1 = 0;
    uint64_t mant2 = 0;
    int exp1 = 0;
    int exp2 = 0;

    get_full_mant_exp(a, mant1, exp1, fmt);
    get_full_mant_exp(b, mant2, exp2, fmt);

    int extra = static_cast<int>(result.mant_bits) + 4;
    uint64_t dividend = mant1 << extra;
    uint64_t mant_result = dividend / mant2;
    uint64_t remainder = dividend % mant2;
    int exp_result = exp1 - exp2;

    uint64_t hidden = 1ull << result.mant_bits;
    uint64_t target = hidden << 4;
    int min_exp = 1 - static_cast<int>(result.bias) - static_cast<int>(result.mant_bits);

    while (mant_result >= (target << 1)){
        remainder |= (mant_result & 1ull);
        mant_result >>= 1;
        exp_result++;
    }
    while (mant_result < target && exp_result > min_exp){
        mant_result <<= 1;
        exp_result--;
    }

    int biased_exp = exp_result + static_cast<int>(result.bias);

    if (biased_exp <= 0){
        int total_shift = 4 + (1 - biased_exp);
        RoundingInfo info{};

        if (total_shift < 64){
            info = extract_rounding_info(mant_result, total_shift, remainder != 0);
            mant_result >>= total_shift;
        }else{
            info.sticky = (mant_result != 0) || (remainder != 0);
            mant_result = 0;
        }

        if (should_round_up(result.sign, info, mant_result, rounding)){
            mant_result++;
            if (mant_result >= hidden){
                biased_exp = 1;
                mant_result &= result.mant_max;
            }
        }

        if (mant_result == 0 && biased_exp <= 0) return make_zero(result, result.sign);

        if (biased_exp <= 0){
            biased_exp = 0;
        }

        result.exp = static_cast<uint32_t>(biased_exp);
        result.mant = static_cast<uint32_t>(mant_result & result.mant_max);
        pack_result(result);
        return result;
    }

    RoundingInfo info{};
    info.guard = ((mant_result >> 3) & 1ull) != 0;
    info.round = ((mant_result >> 2) & 1ull) != 0;
    info.sticky = ((mant_result & 3ull) != 0) || (remainder != 0);

    mant_result >>= 4;

    if (should_round_up(result.sign, info, mant_result, rounding)){
        mant_result++;
        if (mant_result >= (hidden << 1)){
            mant_result >>= 1;
            biased_exp++;
        }
    }

    if (biased_exp >= static_cast<int>(result.exp_max)) return make_overflow_result(result, result.sign, rounding, fmt);

    result.exp = static_cast<uint32_t>(biased_exp);
    result.mant = static_cast<uint32_t>(mant_result & result.mant_max);
    pack_result(result);
    return result;
}

My_float float_mad(const My_float& a, const My_float& b, const My_float& c, int rounding, const std::string& fmt){
    return float_add(float_mul(a, b, rounding, fmt), c, rounding, fmt);
}

My_float float_fma(const My_float& a, const My_float& b, const My_float& c, int rounding, const std::string& fmt){
    My_float result{};
    if (handle_special_fma(a, b, c, result, rounding, fmt)) return result;
    copy_format(result, a);
    uint64_t mant1 = 0;
    uint64_t mant2 = 0;
    uint64_t mant3 = 0;
    int exp1 = 0;
    int exp2 = 0;
    int exp3 = 0;

    get_full_mant_exp(a, mant1, exp1, fmt);
    get_full_mant_exp(b, mant2, exp2, fmt);
    get_full_mant_exp(c, mant3, exp3, fmt);

    uint64_t mant_prod = mant1 * mant2;
    int exp_prod = exp1 + exp2;
    uint32_t sign_prod = a.sign ^ b.sign;

    int prod_bits = 2 * static_cast<int>(result.mant_bits);
    uint64_t prod_hidden = 1ull << prod_bits;
    uint64_t mant3_ext = mant3 << result.mant_bits;

    const int extra_bits = 3;

    mant_prod <<= extra_bits;
    mant3_ext <<= extra_bits;

    int exp_result = std::max(exp_prod, exp3);
    uint64_t aligned_prod = mant_prod;
    uint64_t aligned_add = mant3_ext;
    bool sticky_prod = false;
    bool sticky_add = false;

    if (exp_prod < exp_result){
        shift_right_sticky(aligned_prod, exp_result - exp_prod, sticky_prod);
    }

    if (exp3 < exp_result){
        shift_right_sticky(aligned_add, exp_result - exp3, sticky_add);
    }

    uint64_t mant_result = 0;
    bool sticky_result = false;

    if (sign_prod == c.sign){
        result.sign = sign_prod;
        mant_result = aligned_prod + aligned_add;
        sticky_result = sticky_prod || sticky_add;
    }else{
        bool prod_greater = false;
        if (aligned_prod != aligned_add){
            prod_greater = aligned_prod > aligned_add;
        }else {
            prod_greater = sticky_prod && !sticky_add;
        }
        if (prod_greater || (aligned_prod == aligned_add && !sticky_add)){
            result.sign = sign_prod;
            mant_result = aligned_prod - aligned_add;
            if (sticky_add && !sticky_prod){
                mant_result--;
                sticky_result = true;
            }else {
                sticky_result = sticky_prod || sticky_add;
            }
        }else {
            result.sign = c.sign;
            mant_result = aligned_add - aligned_prod;

            if (sticky_prod && !sticky_add){
                mant_result--;
                sticky_result = true;
            }else {
                sticky_result = sticky_prod || sticky_add;
            }
        }
    }

    if (mant_result == 0 && !sticky_result) return make_zero(result, (rounding == 3) ? 1u : 0u);

    uint64_t target = prod_hidden << extra_bits;
    int min_exp = 1 - static_cast<int>(result.bias) - static_cast<int>(result.mant_bits);

    while (mant_result >= (target << 1)){
        sticky_result = sticky_result || ((mant_result & 1ull) != 0);
        mant_result >>= 1;
        exp_result++;
    }

    while (mant_result != 0 && mant_result < target && exp_result > min_exp){
        mant_result <<= 1;
        exp_result--;
    }

    int biased_exp = exp_result + static_cast<int>(result.bias);
    int total_shift = static_cast<int>(result.mant_bits) + extra_bits;

    if (biased_exp <= 0){
        total_shift += 1 - biased_exp;
        biased_exp = 0;
    }

    RoundingInfo info{};
    uint64_t final_mant = 0;

    if (total_shift < 64){
        info = extract_rounding_info(mant_result, total_shift, sticky_result);
        final_mant = (total_shift <= 0) ? mant_result : (mant_result >> total_shift);
    }else {
        info.sticky = sticky_result || (mant_result != 0);
        final_mant = 0;
    }

    uint64_t hidden = 1ull << result.mant_bits;

    if (should_round_up(result.sign, info, final_mant, rounding)){
        final_mant++;

        if (biased_exp == 0 && final_mant >= hidden){
            biased_exp = 1;
            final_mant &= result.mant_max;
        }else if (biased_exp > 0 && final_mant >= (hidden << 1)){
            final_mant >>= 1;
            biased_exp++;
        }
    }

    if (biased_exp >= static_cast<int>(result.exp_max)) return make_overflow_result(result, result.sign, rounding, fmt);
    if (final_mant == 0 && biased_exp == 0) return make_zero(result, result.sign);
    if (biased_exp > 0){
        final_mant &= result.mant_max;
    }

    result.exp = static_cast<uint32_t>(biased_exp);
    result.mant = static_cast<uint32_t>(final_mant & result.mant_max);
    pack_result(result);
    return result;
}

My_float do_operation(const My_float& a, const My_float& b, char op, int rounding, const std::string& fmt){
    switch (op){
        case '+': return float_add(a, b, rounding, fmt);
        case '-': return float_sub(a, b, rounding, fmt);
        case '*': return float_mul(a, b, rounding, fmt);
        case '/': return float_div(a, b, rounding, fmt);
        default: return make_nan(a, fmt);
    }
}

My_float do_ternary_operation(const My_float& a, const My_float& b, const My_float& c, const std::string& op, int rounding, const std::string& fmt){
    if (op == "mad") return float_mad(a, b, c, rounding, fmt);
    if (op == "fma") return float_fma(a, b, c, rounding, fmt);
    return make_nan(a, fmt);
}

bool parse_hex(const char* str, uint32_t& value){
    if (str == nullptr || *str == '\0') return false;
    if (str[0] == '0' && (str[1] == 'x' || str[1] == 'X')){
        str += 2;
    }
    char* endptr = nullptr;
    value = static_cast<uint32_t>(std::strtoul(str, &endptr, 16));
    return *endptr == '\0';
}

bool ops_supported(const std::string& fmt){
    return fmt == "h" || fmt == "s";
}

int main(int argc, char** argv){
    if (argc != 4 && argc != 6 && argc != 7){
        std::cerr << "Wrong amount of arguments";
        return 2;
    }
    std::string fmt = argv[1];

    if (fmt != "h" && fmt != "s" && fmt != "e5m2" && fmt != "e4m3fn" && fmt != "e2m1"){
        std::cerr << "Wrong type of format";
        return 3;
    }
    int rounding = std::atoi(argv[2]);
    if (rounding < 0 || rounding > 3){
        std::cerr << "Wrong rounding";
        return 4;
    }
    uint32_t value1 = 0;
    uint32_t value2 = 0;
    uint32_t value3 = 0;

    switch(argc){
        case 4: {
            if (!parse_hex(argv[3], value1)){
                std::cerr << "Wrong number format";
                return 5;
            }
            My_float result = do_fmt(value1, fmt);
            print_float(result, fmt);
            return 0;
        }
        case 6:{
            if (!parse_hex(argv[4], value1) || !parse_hex(argv[5], value2)){
                std::cerr << "Wrong number format";
                return 5;
            }
            std::string op = argv[3];
            if (op.length() != 1 || std::string("+-*/").find(op[0]) == std::string::npos){
                std::cerr << "Wrong operation";
                return 4;
            }
            if (!ops_supported(fmt)){
                std::cerr << "Operations not supported for this format";
                return 6;
            }
            My_float number1 = do_fmt(value1, fmt);
            My_float number2 = do_fmt(value2, fmt);
            My_float result = do_operation(number1, number2, op[0], rounding, fmt);
            print_float(result, fmt);
            return 0;
        }
        case 7:{
            if (!parse_hex(argv[4], value1) || !parse_hex(argv[5], value2) || !parse_hex(argv[6], value3)){
                std::cerr << "Wrong number format";
                return 5;
            }
            std::string op = argv[3];
        
            if (op != "mad" && op != "fma"){
                std::cerr << "Wrong operation";
                return 4;
            }
        
            if (!ops_supported(fmt)){
                std::cerr << "Operations not supported for this format";
                return 6;
            }
            My_float number1 = do_fmt(value1, fmt);
            My_float number2 = do_fmt(value2, fmt);
            My_float number3 = do_fmt(value3, fmt);
            My_float result = do_ternary_operation(number1, number2, number3, op, rounding, fmt);
            print_float(result, fmt);
            return 0;
        }
    }
}