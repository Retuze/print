#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define is_digit(c) ((c) >= '0' && (c) <= '9')

static int skip_atoi(const char **fmtp)
{
    int i = 0;
    while (is_digit(**fmtp))
        i = i * 10 + *((*fmtp)++) - '0';
    return i;
}

#define ZEROPAD 1  /* pad with zero */
#define SIGN 2     /* unsigned/signed long */
#define PLUS 4     /* show plus */
#define SPACE 8    /* space if plus */
#define LEFT 16    /* left justified */
#define SPECIAL 32 /* 0x */
#define SMALL 64   /* use 'abcdef' instead of 'ABCDEF' */
#define LONG 128   // if long long

// 一般编译器都支持32位除法，不一定支持64位除法，故此处使用软件处理，提高可移植性
// 仅用于处理32位除法，64位的实现在else分支
unsigned int do_div_10(unsigned int *n)
{
    unsigned int t = *n % 10;
    *n = *n / 10;
    return t;
}

// 利用2的幂次方特性，把16进制，8进制下64位除法统一转换32位除法
unsigned int do_div_16_8(unsigned long long *n, int base)
{
    unsigned int t = base == 16 ? 28 : 29;
    unsigned int low = *n;
    unsigned int hign = (*n) >> 32;
    unsigned int mod = ((*n) & (base == 16 ? 15 : 7)); // a & (base - 1)
    unsigned long long tmp = (unsigned long long)(1 << t) * hign + low / base;
    *n = tmp;
    return mod;
}
static char *number(char *str, long long num, int base, int size, int precision, int type)
{
    char c, sign, tmp[36];
    const char *digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    int i;
    if (type & SMALL)
        digits = "0123456789abcdefghijklmnopqrstuvwxyz";
    if (type & LEFT)
        type &= ~ZEROPAD;
    if (base < 2 || base > 36)
        return 0;
    c = (type & ZEROPAD) ? '0' : ' ';
    if (type & SIGN && num < 0)
    {
        sign = '-';
        num = -num;
    }
    else
        sign = ((type & PLUS) && (base == 10)) ? '+' : (((type & SPACE) && (base == 10)) ? ' ' : 0);
    if (sign)
        size--;
    if (type & SPECIAL)
    {
        if (base == 16)
            size -= 2;
        else if (base == 8)
        {
            size--;
        }
    }
    i = 0;
    if (num == 0)
        tmp[i++] = '0';
    if (base == 16 || base == 8)
    {
        if (!(type & LONG))
        {
            int *p = (int *)&num;
            *(++p) = 0;
        }
        while (num != 0)
            tmp[i++] = digits[do_div_16_8((unsigned long long *)&num, base)];
    }
    if (base == 10)
    {
        if (!(type & LONG))
        {
            while (num != 0)
                tmp[i++] = digits[do_div_10((unsigned int *)&num)];
        }
        else
        {
            unsigned int low = num;
            unsigned int hign = num >> 32;
            while (low > 0)
            {
                tmp[i++] = ((hign % 10) * 6 + low % 10) % 10 + '0';
                low = 429496729 * (hign % 10) + low / 10 + ((hign % 10) * 6 + low % 10) / 10;
                hign = hign / 10;
            }
        }
    }
    if (i > precision)
        precision = i;
    size -= precision;
    if (!(type & (ZEROPAD + LEFT)))
        while (size-- > 0)
            *str++ = ' ';
    if (sign)
        *str++ = sign;
    if (type & SPECIAL)
    {
        if (base == 8)
        {
            // *str++ = '0';
        }
        else if (base == 16)
        {
            *str++ = '0';
            *str++ = digits[33];
        }
    }
    if (!(type & LEFT))
        while (size-- > 0)
            *str++ = ' ';
    while (i < precision--)
    {
        *str++ = '0';
    }

    while (i-- > 0)
        *str++ = tmp[i];
    while (size-- > 0)
        *str++ = ' ';
    return str;
}

int nano_vsprintf(char *buf, const char *fmt, va_list args)
{
    int len;
    char *str;
    char *s;
    int *ip;
    int flags;       /* flags to number() */
    int field_width; /* width of output field */
    int precision;   /* min. # of digits for integers; max number of chars for from string */
    int qualifier;   /* 'h', 'l', or 'L' for integer fields */
    for (str = buf; *fmt; ++fmt)
    {
        if (*fmt != '%')
        {
            *str++ = *fmt;
            continue;
        }

        /* process flags */
        flags = 0;
    repeat:
        ++fmt; /* this also skips first '%' */
        switch (*fmt)
        {
        case '-':
            flags |= LEFT;
            goto repeat;
        case '+':
            flags |= PLUS;
            goto repeat;
        case ' ':
            flags |= SPACE;
            goto repeat;
        case '#':
            flags |= SPECIAL;
            goto repeat;
        case '0':
            flags |= ZEROPAD;
            goto repeat;
        }

        /* get field width */
        field_width = -1;
        if (is_digit(*fmt))
            field_width = skip_atoi(&fmt);
        else if (*fmt == '*')
        {
            ++fmt;
            /* it's the next argument */
            field_width = va_arg(args, int);
            if (field_width < 0)
            {
                field_width = -field_width;
                flags |= LEFT;
            }
        }
        /* get the precision */
        precision = -1;
        if (*fmt == '.')
        {
            ++fmt;
            if (is_digit(*fmt))
                precision = skip_atoi(&fmt);
            else if (*fmt == '*')
            {
                ++fmt;
                /* it's the next argument */
                precision = va_arg(args, int);
            }
            if (precision < 0)
                precision = 0;
        }

        /* get the conversion qualifier */
        qualifier = -1;
        if (*fmt == 'h')
        {
            qualifier = *fmt;
            ++fmt;
        }
        else if (*fmt == 'l')
        {
            qualifier = *fmt;
            fmt++;
            if (*fmt == 'l')
            {
                qualifier = 'm';
                flags |= LONG;
                fmt++;
            }
        }

        switch (*fmt)
        {
        case 'c':
            if (!(flags & LEFT))
                while (--field_width > 0)
                    *str++ = (flags & ZEROPAD) ? '0' : ' ';
            *str++ = (unsigned char)va_arg(args, int);
            while (--field_width > 0)
                *str++ = (flags & ZEROPAD) ? '0' : ' ';
            break;

        case 's':
            s = va_arg(args, char *);
            len = strlen(s);
            if (precision < 0)
                precision = len;
            else if (len > precision)
                len = precision;

            if (!(flags & LEFT))
                while (len < field_width--)
                    *str++ = (flags & ZEROPAD) ? '0' : ' ';
            for (int i = 0; i < len; ++i)
                *str++ = *s++;
            while (len < field_width--)
                *str++ = (flags & ZEROPAD) ? '0' : ' ';
            break;

        case 'o':
            if (qualifier == 'h')
                str = number(str, va_arg(args, unsigned int), 8, field_width, precision, flags);
            else if (qualifier == 'l')
                str = number(str, va_arg(args, unsigned long), 8, field_width, precision, flags);
            else if (qualifier == 'm')
                str = number(str, va_arg(args, unsigned long long), 8, field_width, precision, flags);
            else
                str = number(str, va_arg(args, unsigned int), 8, field_width, precision, flags);
            break;

        case 'p':
            if (field_width == -1)
            {
                field_width = 8;
                flags |= ZEROPAD;
            }
            str = number(str, (unsigned long)va_arg(args, unsigned long), 16, field_width, precision, flags);
            break;

        case 'x':
            flags |= SMALL;
        case 'X':
            if (qualifier == 'h')
                str = number(str, va_arg(args, unsigned int), 16, field_width, precision, flags);
            else if (qualifier == 'l')
                str = number(str, va_arg(args, unsigned long), 16, field_width, precision, flags);
            else if (qualifier == 'm') // %llx
                str = number(str, va_arg(args, unsigned long long), 16, field_width, precision, flags);
            else
                str = number(str, va_arg(args, unsigned int), 16, field_width, precision, flags);
            break;

        case 'd':
        case 'i':
            flags |= SIGN;
            if (qualifier == 'h')
                str = number(str, va_arg(args, int), 10, field_width, precision, flags);
            else if (qualifier == 'l')
                str = number(str, va_arg(args, long), 10, field_width, precision, flags);
            else if (qualifier == 'm')
                str = number(str, va_arg(args, long long), 10, field_width, precision, flags);
            else
                str = number(str, va_arg(args, int), 10, field_width, precision, flags);
            break;

        case 'u':
            if (qualifier == 'h')
                str = number(str, va_arg(args, unsigned int), 10, field_width, precision, flags);
            else if (qualifier == 'l')
                str = number(str, va_arg(args, unsigned long), 10, field_width, precision, flags);
            else if (qualifier == 'm')
                str = number(str, va_arg(args, unsigned long long), 10, field_width, precision, flags);
            else
                str = number(str, va_arg(args, unsigned int), 10, field_width, precision, flags);
            break;

        case 'n':
            ip = va_arg(args, int *);
            *ip = (str - buf);
            break;

        default:
            if (*fmt != '%')
                *str++ = '%';
            if (*fmt)
                *str++ = *fmt;
            else
                --fmt;
            break;
        }
    }
    *str = '\0';
    return str - buf;
}

int nano_sprintf(char *buf, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    int ret = nano_vsprintf(buf, fmt, args);
    va_end(args);
    return ret;
}

int nano_printf(const char *fmt, ...)
{
    char buf[120];
    va_list args;
    va_start(args, fmt);
    int ret = nano_vsprintf(buf, fmt, args);
    va_end(args);
    printf("%s", buf);
    return ret;
}

// 生成随机字符串，长度在1到60个字符之间
char *generate_random_string()
{
    const char charset[] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    static char str[120] = {0};
    size_t max_index = sizeof(charset) - 1;
    int length = rand() % 64; // 随机长度
    for (int i = 0; i < length; ++i)
    {
        str[i] = charset[rand() % max_index];
    }
    str[length] = '\0'; // 字符串结尾
    return str;
}

// 生成随机浮点数，整数部分1到6位，小数部分0到6位
double generate_random_float()
{
    int integer_part = rand() % 1000000 + 1; // 1到6位的整数部分
    int fraction_digits = rand() % 7;        // 0到6位的小数部分
    int fraction_part = 0;

    // 生成小数部分
    for (int i = 0; i < fraction_digits; ++i)
    {
        fraction_part = fraction_part * 10 + rand() % 10;
    }

    // 组合成浮点数
    double random_float = (double)integer_part + ((double)fraction_part / pow(10, fraction_digits));
    return random_float;
}

// 生成随机字符
char generate_random_char()
{
    return rand() % 95 + 32;
}

// 生成随机整数，1到6位
int generate_random_int()
{
    int lower_bound = 1;
    int upper_bound = 1000000; // 1到6位的整数范围
    return rand() % (upper_bound - lower_bound + 1) + lower_bound;
}

char *generate_random_fmt(char type)
{
    static char fmt[24] = {0};
    char flag[] = {' ', '+', '-', '0', '#'};
    char *fmt_p = fmt;
    *fmt_p++ = '%';
    *fmt_p++ = rand() % 6 ? flag[rand() % 5] : 0 + '0';
    int width = rand() % 8;
    *fmt_p++ = width + '0';
    *fmt_p++ = '.';
    int precision = rand() % 8;
    *fmt_p++ = precision + '0';
    *fmt_p++ = type;
    *fmt_p++ = 0;
    return fmt;
}
#define MAX_EPOCH 100
int main()
{
    // printf("%#5.6o\n", 15591);
    // nano_printf("%#5.6o\n", 15591);
    // return 0;
    srand(100);
    // srand(time(NULL));
    for (int i = 0; i < MAX_EPOCH; i++)
    {
        char std_buf[240] = {0};
        char mtd_buf[240] = {0};
        char *fmt;
        char c;
        int number;
        int l;
        double decimal;
        char *str;

        // memset(std_buf, 0, 240);
        // memset(mtd_buf, 0, 240);
        // fmt = generate_random_fmt('d');
        // number = generate_random_int();
        // l = sprintf(std_buf, fmt, number);
        // // printf("fmt :%s,number:%d\n",fmt,number);
        // // printf("std_buf : %s\n", std_buf);
        // nano_sprintf(mtd_buf, fmt, number);
        // if (memcmp(std_buf, mtd_buf, l) != 0)
        // {
        //     printf("error :%d\n", i);
        //     printf("fmt :%s,number:%d\n", fmt, number);
        //     printf("std_buf : %s\n", std_buf);
        //     printf("mtd_buf : %s\n", mtd_buf);
        //     printf("-----------------------------\n");
        // }

        // memset(std_buf, 0, 240);
        // memset(mtd_buf, 0, 240);
        // fmt = generate_random_fmt('x');
        // number = generate_random_int();
        // l = sprintf(std_buf, fmt, number);
        // // printf("fmt :%s,number:%d\n",fmt,number);
        // // printf("std_buf : %s\n", std_buf);
        // nano_sprintf(mtd_buf, fmt, number);
        // if (memcmp(std_buf, mtd_buf, l) != 0)
        // {
        //     printf("error :%d\n", i);
        //     printf("fmt :%s,number:%d\n", fmt, number);
        //     printf("std_buf : %s\n", std_buf);
        //     printf("mtd_buf : %s\n", mtd_buf);
        //     printf("-----------------------------\n");
        // }
        static int error_tick=0;
        memset(std_buf, 0, 240);
        memset(mtd_buf, 0, 240);
        fmt = generate_random_fmt('o');
        number = generate_random_int();
        l = sprintf(std_buf, fmt, number);
        // printf("fmt :%s,number:%d\n",fmt,number);
        // printf("std_buf : %s\n", std_buf);
        nano_sprintf(mtd_buf, fmt, number);
        if (memcmp(std_buf, mtd_buf, l) != 0)
        {
            printf("error :%d\n", error_tick++);
            printf("fmt :%s,number:%d\n", fmt, number);
            printf("std_buf : %s\n", std_buf);
            printf("mtd_buf : %s\n", mtd_buf);
            printf("-----------------------------\n");
        }

        // memset(std_buf, 0, 240);
        // memset(mtd_buf, 0, 240);
        // fmt = generate_random_fmt('s');
        // str = generate_random_string();
        // l = sprintf(std_buf, fmt, str);
        // nano_sprintf(mtd_buf, fmt, str);
        // // printf("fmt :%s,str:%s\n", fmt, str);
        // // printf("std_buf : %s\n", std_buf);
        // if (memcmp(std_buf, mtd_buf, l) != 0)
        // {
        //     printf("error :%d\n", i);
        //     printf("fmt :%s,str:%s\n", fmt, str);
        //     printf("std_buf : %s\n", std_buf);
        //     printf("mtd_buf : %s\n", mtd_buf);
        //     printf("-----------------------------\n");
        // }

        // memset(std_buf, 0, 240);
        // memset(mtd_buf, 0, 240);
        // fmt = generate_random_fmt('c');
        // c = generate_random_char();
        // l = sprintf(std_buf, fmt, c);
        // nano_sprintf(mtd_buf, fmt, c);
        // // printf("fmt :%s,str:%s\n", fmt, c);
        // // printf("std_buf : %s\n", std_buf);
        // if (memcmp(std_buf, mtd_buf, l) != 0)
        // {
        //     printf("error :%d\n", i);
        //     printf("fmt :%s,c :%c\n", fmt, c);
        //     printf("std_buf : %s\n", std_buf);
        //     printf("mtd_buf : %s\n", mtd_buf);
        //     printf("-----------------------------\n");
        // }

        // memset(std_buf, 0, 240);
        // memset(mtd_buf, 0, 240);
        // fmt = generate_random_fmt('f');
        // decimal = generate_random_float();
        // l = sprintf(std_buf, fmt, decimal);
        // sprint(mtd_buf, fmt, decimal);
        // if (memcmp(std_buf, mtd_buf, l) != 0)
        // {
        //     printf("flase %d\n", i);
        //     printf("std_buf : %s\n", std_buf);
        //     printf("mtd_buf : %s\n", mtd_buf);
        // }
    }
}