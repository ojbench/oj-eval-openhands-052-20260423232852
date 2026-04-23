
#include<iostream>
#include<algorithm>
#include<cstring>
#include<vector>
#include<stack>
#include<cctype>

class term { //项
 public:
    int a, b, c, d;

    term() {}
    term(int _a, int _b, int _c, int _d) { // 你可以用 term t(1, 2, 3, 4); 创建一个表示 x^2sin^3xcos^4x 的term
        a = _a; b = _b; c = _c; d = _d;
    }
    bool operator == (const term &obj) const { // 相等表示是同类项
        return b == obj.b && c == obj.c && d == obj.d;
    }
    bool operator != (const term &obj) const {
        return b != obj.b || c != obj.c || d != obj.d;
    }
    
    // 用于排序的比较运算符
    bool operator < (const term &obj) const {
        if (b != obj.b) return b > obj.b; // x指数降序
        if (c != obj.c) return c > obj.c; // sin指数降序
        return d > obj.d; // cos指数降序
    }
};

class poly { //多项式
 public:
    int n;
    term *t;

    poly() {n = 0; t = NULL;}
    poly(int _n) { // 构造函数
        n = _n;
        t = new term[n];
    }
    poly(const poly &p) {
        n = p.n;
        t = new term[n];
        for (int i = 0; i < n; ++i) {
            t[i] = p.t[i];
        }
    }
    
    void simplify() { // 合并同类项 
        if (n <= 1) return;
        
        // 先排序
        std::sort(t, t + n);
        
        // 合并同类项
        std::vector<term> merged;
        int i = 0;
        while (i < n) {
            term current = t[i];
            int j = i + 1;
            while (j < n && t[j] == current) {
                current.a += t[j].a;
                j++;
            }
            if (current.a != 0) { // 只保留系数不为0的项
                merged.push_back(current);
            }
            i = j;
        }
        
        // 更新多项式
        delete []t;
        n = merged.size();
        t = new term[n];
        for (int i = 0; i < n; ++i) {
            t[i] = merged[i];
        }
    }
    
    poly operator + (const poly &obj) const { // 对 poly 重载加法运算符
        poly ans(n + obj.n);
        for (int i = 0; i < n; ++i) ans.t[i] = t[i];
        for (int i = 0; i < obj.n; ++i) ans.t[i + n] = obj.t[i];
        ans.simplify();
        return ans;
    }
    
    poly operator - (const poly &obj) const { // 对 poly 重载减法运算符
        poly ans(n + obj.n);
        for (int i = 0; i < n; ++i) ans.t[i] = t[i];
        for (int i = 0; i < obj.n; ++i) {
            ans.t[i + n] = obj.t[i];
            ans.t[i + n].a *= -1;
        }
        ans.simplify();
        return ans;
    }
    
    poly operator * (const poly &obj) const { // 对 poly 重载乘法运算符
        poly ans(n * obj.n);
        int idx = 0;
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < obj.n; ++j) {
                ans.t[idx].a = t[i].a * obj.t[j].a;
                ans.t[idx].b = t[i].b + obj.t[j].b;
                ans.t[idx].c = t[i].c + obj.t[j].c;
                ans.t[idx].d = t[i].d + obj.t[j].d;
                idx++;
            }
        }
        ans.simplify();
        return ans;
    }
    
    poly& operator = (const poly &obj) {
        if (&obj == this) return *this;
        n = obj.n;
        if (t != NULL) delete []t;
        t = new term[n];
        for (int i = 0; i < n; ++i) {
            t[i] = obj.t[i];
        }
        return *this;
    }
    
    poly derivate() const {  // 注意,你需要以"只读模式"访问当前的poly,并返回一个求导后的poly
        poly ans(n);
        for (int i = 0; i < n; ++i) {
            term current = t[i];
            
            // 常数项导数为0
            if (current.b == 0 && current.c == 0 && current.d == 0) {
                ans.t[i].a = 0;
                continue;
            }
            
            // 计算导数
            if (current.b > 0) {
                // x^b 的导数: b * x^(b-1)
                ans.t[i].a = current.a * current.b;
                ans.t[i].b = current.b - 1;
                ans.t[i].c = current.c;
                ans.t[i].d = current.d;
            } else if (current.c > 0) {
                // sin^c x 的导数: c * sin^(c-1) x * cos x
                ans.t[i].a = current.a * current.c;
                ans.t[i].b = current.b;
                ans.t[i].c = current.c - 1;
                ans.t[i].d = current.d + 1;
            } else if (current.d > 0) {
                // cos^d x 的导数: -d * sin x * cos^(d-1) x
                ans.t[i].a = -current.a * current.d;
                ans.t[i].b = current.b;
                ans.t[i].c = current.c + 1;
                ans.t[i].d = current.d - 1;
            }
        }
        ans.simplify();
        return ans;
    }
    
    ~poly() { // 析构函数,释放内存
        if (t != NULL) delete []t;
    }
};

class frac { //分式
 public:
    poly p, q;

    frac() {}
    frac(int x) {
        p = poly(1);
        p.t[0] = term(x, 0, 0, 0);
        q = poly(1);
        q.t[0] = term(1, 0, 0, 0);
    }
    frac(term _p) {
        q = poly(1);
        q.t[0] = term(1, 0, 0, 0);
        p = poly(1);
        p.t[0] = _p;
    }
    frac(poly _p, poly _q) : p(_p), q(_q) {}
    frac(poly _p) : p(_p), q(poly(1)) {
        q.t[0] = term(1, 0, 0, 0);
    }

    frac operator + (const frac &obj) const {
        return frac(p * obj.q + q * obj.p, q * obj.q); // p1/q1+p2/q2=(p1*q2+p2*q1)/(q1*q2)
    }
    
    frac operator - (const frac &obj) const {
        return frac(p * obj.q - q * obj.p, q * obj.q); // p1/q1-p2/q2=(p1*q2-p2*q1)/(q1*q2)
    }
    
    frac operator * (const frac &obj) const {
        return frac(p * obj.p, q * obj.q); // p1/q1*p2/q2=(p1*p2)/(q1*q2)
    }
    
    frac operator / (const frac &obj) const {
        return frac(p * obj.q, q * obj.p); // p1/q1/p2/q2=(p1*q2)/(q1*p2)
    }
    
    frac derivate() const { // 对分式求导
        // (f/g)' = (f'g - g'f)/(g*g)
        return frac(p.derivate() * q - q.derivate() * p, q * q);
    }
    
    void output() {
        // todo
    }
};

// 表达式解析器
class ExpressionParser {
private:
    std::string s;
    int pos;
    int n;
    
public:
    ExpressionParser(const std::string &str) : s(str), pos(0), n(str.length()) {}
    
    char peek() {
        if (pos < n) return s[pos];
        return '\0';
    }
    
    char next() {
        if (pos < n) return s[pos++];
        return '\0';
    }
    
    void skipWhitespace() {
        while (pos < n && isspace(s[pos])) pos++;
    }
    
    // 解析项
    term parseTerm() {
        int a = 1, b = 0, c = 0, d = 0;
        
        // 解析系数
        if (isdigit(peek())) {
            a = 0;
            while (isdigit(peek())) {
                a = a * 10 + (next() - '0');
            }
        }
        
        // 解析变量部分
        while (pos < n) {
            if (peek() == 'x') {
                next();
                b = 1;
                if (peek() == '^') {
                    next();
                    b = 0;
                    while (isdigit(peek())) {
                        b = b * 10 + (next() - '0');
                    }
                }
            } else if (pos + 2 < n && s[pos] == 's' && s[pos+1] == 'i' && s[pos+2] == 'n') {
                pos += 3;
                c = 1;
                if (peek() == '^') {
                    next();
                    c = 0;
                    while (isdigit(peek())) {
                        c = c * 10 + (next() - '0');
                    }
                }
                if (peek() == 'x') next(); // 跳过x
            } else if (pos + 2 < n && s[pos] == 'c' && s[pos+1] == 'o' && s[pos+2] == 's') {
                pos += 3;
                d = 1;
                if (peek() == '^') {
                    next();
                    d = 0;
                    while (isdigit(peek())) {
                        d = d * 10 + (next() - '0');
                    }
                }
                if (peek() == 'x') next(); // 跳过x
            } else {
                break;
            }
        }
        
        return term(a, b, c, d);
    }
    
    // 解析单个项（作为分式）
    frac parseSingleTerm() {
        term t = parseTerm();
        poly p(1);
        p.t[0] = t;
        return frac(p);
    }
    
    // 解析表达式（使用运算符优先级）
    frac parseExpression() {
        return parseAddSub();
    }
    
private:
    frac parseAddSub() {
        frac left = parseMulDiv();
        
        while (pos < n && (peek() == '+' || peek() == '-')) {
            char op = next();
            frac right = parseMulDiv();
            
            if (op == '+') {
                left = left + right;
            } else {
                left = left - right;
            }
        }
        
        return left;
    }
    
    frac parseMulDiv() {
        frac left = parseFactor();
        
        while (pos < n && (peek() == '*' || peek() == '/')) {
            char op = next();
            frac right = parseFactor();
            
            if (op == '*') {
                left = left * right;
            } else {
                left = left / right;
            }
        }
        
        return left;
    }
    
    frac parseFactor() {
        if (pos < n && peek() == '(') {
            next();
            frac result = parseExpression();
            if (pos < n && peek() == ')') next();
            return result;
        } else if (pos < n && peek() == '-') {
            next();
            frac result = parseFactor();
            return frac(-1) * result;
        } else {
            return parseSingleTerm();
        }
    }
};

// 输出格式化函数
std::string formatTerm(const term &t) {
    std::string result;
    
    // 处理系数
    if (t.b == 0 && t.c == 0 && t.d == 0) {
        // 常数项
        return std::to_string(t.a);
    }
    
    // 处理符号和系数
    if (t.a < 0) {
        result += "-";
    } else if (t.a != 1) {
        result += std::to_string(t.a);
    }
    
    // 处理x部分
    if (t.b > 0) {
        result += "x";
        if (t.b > 1) {
            result += "^" + std::to_string(t.b);
        }
    }
    
    // 处理sin部分
    if (t.c > 0) {
        result += "sin";
        if (t.c > 1) {
            result += "^" + std::to_string(t.c);
        }
        result += "x";
    }
    
    // 处理cos部分
    if (t.d > 0) {
        result += "cos";
        if (t.d > 1) {
            result += "^" + std::to_string(t.d);
        }
        result += "x";
    }
    
    return result;
}

std::string formatPoly(const poly &p) {
    if (p.n == 0) return "0";
    if (p.n == 1) return formatTerm(p.t[0]);
    
    std::string result;
    for (int i = 0; i < p.n; ++i) {
        if (i > 0) {
            if (p.t[i].a >= 0) result += "+";
            result += formatTerm(p.t[i]);
        } else {
            result += formatTerm(p.t[i]);
        }
    }
    
    // 如果有多项，需要加括号
    if (p.n > 1) {
        result = "(" + result + ")";
    }
    
    return result;
}

std::string formatFrac(const frac &f) {
    // 检查特殊情况
    if (f.p.n == 0) return "0";
    
    bool qIsOne = (f.q.n == 1 && f.q.t[0].a == 1 && f.q.t[0].b == 0 && f.q.t[0].c == 0 && f.q.t[0].d == 0);
    
    if (qIsOne) {
        // 分母为1，直接输出分子
        if (f.p.n == 1) {
            return formatTerm(f.p.t[0]);
        } else {
            std::string result;
            for (int i = 0; i < f.p.n; ++i) {
                if (i > 0) {
                    if (f.p.t[i].a >= 0) result += "+";
                    result += formatTerm(f.p.t[i]);
                } else {
                    result += formatTerm(f.p.t[i]);
                }
            }
            return result;
        }
    }
    
    // 正常分式输出
    std::string numerator, denominator;
    
    // 格式化分子
    if (f.p.n == 1) {
        numerator = formatTerm(f.p.t[0]);
    } else {
        numerator = "(";
        for (int i = 0; i < f.p.n; ++i) {
            if (i > 0) {
                if (f.p.t[i].a >= 0) numerator += "+";
                numerator += formatTerm(f.p.t[i]);
            } else {
                numerator += formatTerm(f.p.t[i]);
            }
        }
        numerator += ")";
    }
    
    // 格式化分母
    if (f.q.n == 1) {
        denominator = formatTerm(f.q.t[0]);
    } else {
        denominator = "(";
        for (int i = 0; i < f.q.n; ++i) {
            if (i > 0) {
                if (f.q.t[i].a >= 0) denominator += "+";
                denominator += formatTerm(f.q.t[i]);
            } else {
                denominator += formatTerm(f.q.t[i]);
            }
        }
        denominator += ")";
    }
    
    return numerator + "/" + denominator;
}

void solve(char *s, int n) {
    std::string str(s, n);
    ExpressionParser parser(str);
    
    frac result = parser.parseExpression();
    frac derivative = result.derivate();
    
    // 输出结果
    std::cout << formatFrac(result) << std::endl;
    std::cout << formatFrac(derivative) << std::endl;
}

int main() {
    std::string str;
    std::cin >> str;
    int n = str.length();
    char *s = new char[n + 2]{0};
    for (int i = 0; i < n; ++i) s[i] = str[i];
    solve(s, n);
    delete []s;
    return 0;
}
