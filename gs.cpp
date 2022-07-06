#include<iostream>
#include<vector>
#include<string>
#include<ctime>
#include<math.h>
using namespace std;
#define type long long

long long curve_a, curve_b, curve_p, curve_n, curve_x, curve_y;

type add_mod(type a, type b, type mod) {
	return (a + b) % mod;
}
type pre_mod(type a, type b, type mod) {
	if (a < b) {
		
		return (a + mod - b) % mod;
	}
	return (a - b) % mod;
}
type mul_mod(type a, type b, type mod) {
	type ans = 0;
	while (b) {
		if (b & 1)
			ans = (ans + a) % mod;
		a = (a * 2) % mod;
		b = b / 2;
	}
	return ans;
}
type f_mod(type b, type a) {
	vector<type> c;
	type tmp;
	c.push_back(a);
	c.push_back(b);
	while (b != 1) {
		c.push_back(a % b);
		a = b;
		b = c.back();
	}
	a = c[0];
	b = c[1];
	c.pop_back();
	type x = 0, y = 1;
	while (c.size() > 1) {
		tmp = x;
		x = y;
		y = pre_mod(tmp, mul_mod(y, c[c.size() - 2] / c.back(), a), a);
		c.pop_back();
	}
	return y;
}
class curve {
public:
	bool zero;
	type x, y;
	curve() : x(0), y(0), zero(true) {};
	curve(type num1, type num2) : x(num1), y(num2), zero(false) {};
	curve operator=(const curve& Q) {
		x = Q.x;
		y = Q.y;
		zero = Q.zero;
		return *this;
	}
	bool operator==(const curve& Q) {
		if (this->x == Q.x && this->y == Q.y && zero == Q.zero)
			return true;
		return false;
	}
	curve operator+(const curve& Q) {
		if (Q.zero)
			return *this;
		if (this->zero)
			return Q;
		curve R;
		if (Q.x == x && add_mod(y, Q.y, curve_p) == 0) {
			return R;
		}
		R.zero = false;
		type m;
		if (Q.x == this->x && Q.y == this->y) {
			m = mul_mod(add_mod(mul_mod(mul_mod(x, x, curve_p), 3, curve_p), curve_a, curve_p),
				f_mod(mul_mod(y, 2, curve_p), curve_p), curve_p);
		}
		else {
			m = mul_mod(pre_mod(y, Q.y, curve_p),
				f_mod(pre_mod(x, Q.x, curve_p), curve_p), curve_p);
		}
		R.x = pre_mod(mul_mod(m, m, curve_p), add_mod(x, Q.x, curve_p), curve_p);
		R.y = pre_mod(mul_mod(m, pre_mod(x, R.x, curve_p), curve_p), y, curve_p);
		return R;
	}
	curve operator-(const curve& Q) {
		curve P;
		P.x = Q.x;
		P.y = curve_p - Q.y;
		return (*this + P);
	}
	friend ostream& operator<<(ostream& out, const curve& Q) {
		out << "(" << Q.x << " , " << Q.y << ")";
		return out;
	}
};
class curveset {
public:
	type x;
	int y;
	type a;
	curveset() :x(0), y(3), a(0) {};
	curveset operator=(const curveset& Q) {
		x = Q.x;
		y = Q.y;
		a = Q.a;
		return *this;
	}
};
vector<curve> mul(160);
class GSwalk {
private:
	vector<curveset> tame;
	vector<curveset> wild;
	vector<curve> G;

public:
	hash<type> hash;
	type res_n, num = 0;
	type alphan, Max, M, maxtimes;
	curve curve_P, curve_Q, alpha_N;
	int r;
	void clear() {
		tame.clear();
		G.clear();
		wild.clear();
	}
	curve getnum(type num) {
		curve R;
		int times = 0;
		while (num) {
			if (num & 1) {
				R = R + mul[times];
			}
			++times;
			num = num / 2;
		}
		return R;
	}
	GSwalk(const curve& P, const curve& Q, double alpha) {
		curve_P = P;
		curve_Q = Q;
		maxtimes = 5 * 20;
		Max = curve_n / (maxtimes / 5);
		alphan = (type)(alpha * curve_n);
		alpha_N = getnum(curve_n - alphan);
		M = 13;
		r = 24;
		curve temp = getnum(M), curvet, curvew = Q;
		type numt = 0, numw = 0;
		for (int i = 0; i < r; ++i) {
			G.push_back(temp);
			temp = temp + G[0];
		}
		curveset tametmp, wildtmp;
		while (1) {
			num += 2;
			tametmp = tamewalk(curvet, numt);
			wildtmp = wildwalk(curvew, numw);
			if (wildtmp.y == 3) {
				res_n = curve_n - wildtmp.a;
				goto next;
			}
			//cout << tametmp.a << " " << wildtmp.a << " " << num << "\n";
			if (wildtmp.x == tametmp.x) {
				if (tametmp.y == wildtmp.y) {
					res_n = pre_mod(tametmp.a, wildtmp.a, curve_n);
				}
				else {
					res_n = pre_mod(curve_n - tametmp.a, wildtmp.a, curve_n);
				}
				goto next;
			}
			for (curveset& w : wild) {
				if (tametmp.x == w.x) {
					if (tametmp.y == w.y) {
						//cout << "\n" << 11 << "\n" << tametmp.a << " " << w.a << "\n";
						res_n = pre_mod(tametmp.a, w.a, curve_n);
					}
					else {
						//cout << "\n" << 12 << "\n" << tametmp.a << " " << w.a << "\n";
						res_n = pre_mod(curve_n - tametmp.a, w.a, curve_n);
					}
					goto next;
				}
				if (wildtmp.x == w.x && wildtmp.y != w.y) {
					//cout << "\n" << 2 << "\n" << w.a << " " << wildtmp.a << "\n";
					type temp = add_mod(w.a, wildtmp.a, curve_n);
					if (temp & 1) {
						res_n = (curve_n - temp) / 2;
					}
					else {
						res_n = (2 * curve_n - temp) / 2;
					}
					goto next;
				}
			}
			for (curveset& t : tame) {
				if (wildtmp.x == t.x) {
					if (wildtmp.y == t.y) {
						//cout << "\n" << 31 << "\n" << t.a << " " << wildtmp.a << "\n";
						res_n = pre_mod(t.a, wildtmp.a, curve_n);
					}
					else {
						//cout << "\n" << 32 << "\n" << t.a << " " << wildtmp.a << "\n";
						res_n = pre_mod(curve_n - t.a, wildtmp.a, curve_n);
					}
					goto next;
				}
			}

			tame.push_back(tametmp);
			wild.push_back(wildtmp);
		}
	next:;
		
	}
	void wwalk(curve& R, type& a) {
		int has = hash(a) % r;
		a = add_mod(a, M * ((type)has + 1), curve_n);
		R = R + G[has];
	}
	void twalk(curve& R, type& a) {
		int has = hash(a) % (r + 1);
		if (has != r) {
			a = add_mod(a, M * ((type)has + 1), curve_n);
			R = R + G[has];
		}
		else {
			a = mul_mod(a, 2, curve_n);
			R = R + R;
		}
	}
	curveset tamewalk(curve& R, type& a) {
		if (R.zero) {
			a = (type)((double)rand() / RAND_MAX * curve_n);
			R = getnum(a);
		}
		else {
			twalk(R, a);
		}
		curveset X;
		type times = 0;
		while (R.x > Max && times++ < maxtimes) {
			twalk(R, a);
		}
		if (times >= maxtimes) {
			R = getnum(0);
			a = 0;
			return tamewalk(R, a);
		}
		if (R.zero) {
			return tamewalk(R, a);
		}
		X.x = R.x;
		X.y = (int)(R.y & 1);
		X.a = a;
		return X;
	}
	curveset wildwalk(curve& R, type& a) {
		if (a == 0) {
			a = pre_mod((type)((double)rand() / RAND_MAX * alphan),
				alphan / 2, curve_n);
			R = getnum(a) + curve_Q;
		}
		else {
			wwalk(R, a);
		}
		curveset X;
		type times = 0;
		while (R.x > Max && times++ < maxtimes) {
			wwalk(R, a);
			if ((a > alphan - alphan / 2) && (a < curve_n - alphan / 2)) {
				a = pre_mod(a, alphan, curve_n);
				R = R + alpha_N;
			}
		}
		if (times >= maxtimes) {
			R = curve_Q;
			a = 0;
			return wildwalk(R, a);
		}
		if (R.zero)
			return X;
		X.x = R.x;
		X.y = (int)(R.y & 1);
		X.a = a;
		return X;
	}
};
curve getnum(type num) {
	curve R;
	int times = 0;
	while (num) {
		if (num & 1) {
			R = R + mul[times];
		}
		++times;
		num = num / 2;
	}
	return R;
}
double getalpha(type N) {
	double x = 0.000001, b, min = DBL_MAX, res = 0;
	double temp1, temp2, temp3;
	temp1 = sqrt(3.1415926535 * N);
	temp2 = 1.0 / 4 * pow(1.0 / 2, 1.0 / 4) * pow(N, 1.0 / 4);
	for (int i = 1; i < 1000000; ++i) {
		temp3 = pow(x + 1, 1.0 / 4);
		b = (1.0 / sqrt(2) + (3.0 / sqrt(2) - 2) * x) * temp1 +
			temp2 * (2.0 / x - 20.0 * x + 167.0 / 13 +
			12440.0 / 4641 * temp3 * pow(x, 3.0) + 424.0 / 221 / x +
			43468.0 / 4641 * temp3 * pow(x, 2.0)) + 56612.0 / 4641 * temp3 * x + 
			33428.0 / 4641 * temp3 + 8692.0 / 4641 * temp3 / x + 
			848 / 4641 * (temp3 - 1) / pow(x, 2.0);
		x += 0.000001;
		if (min > b) {
			min = b;
			res = x;
		}
	}
	
	x = 1;
	b = (1.0 / sqrt(2) + (3.0 / sqrt(2) - 2) * x) * temp1 +
		temp2 * (2.0 / x - 20.0 * x + 167.0 / 13 +
			12440.0 / 4641 * temp3 * pow(x, 3.0) + 424.0 / 221 / x +
			43468.0 / 4641 * temp3 * pow(x, 2.0)) + 56612.0 / 4641 * temp3 * x +
		33428.0 / 4641 * temp3 + 8692.0 / 4641 * temp3 / x +
		848 / 4641 * (temp3 - 1) / pow(x, 2.0);
	cout << 1 << " " << b / sqrt(N) << " " << b << "\n";
	x = 0.5;
	b = (1.0 / sqrt(2) + (3.0 / sqrt(2) - 2) * x) * temp1 +
		temp2 * (2.0 / x - 20.0 * x + 167.0 / 13 +
			12440.0 / 4641 * temp3 * pow(x, 3.0) + 424.0 / 221 / x +
			43468.0 / 4641 * temp3 * pow(x, 2.0)) + 56612.0 / 4641 * temp3 * x +
		33428.0 / 4641 * temp3 + 8692.0 / 4641 * temp3 / x +
		848 / 4641 * (temp3 - 1) / pow(x, 2.0);
	cout << 0.5 << " " << b / sqrt(N) << " " << b << "\n";
	x = 0.1;
	b = (1.0 / sqrt(2) + (3.0 / sqrt(2) - 2) * x) * temp1 +
		temp2 * (2.0 / x - 20.0 * x + 167.0 / 13 +
			12440.0 / 4641 * temp3 * pow(x, 3.0) + 424.0 / 221 / x +
			43468.0 / 4641 * temp3 * pow(x, 2.0)) + 56612.0 / 4641 * temp3 * x +
		33428.0 / 4641 * temp3 + 8692.0 / 4641 * temp3 / x +
		848 / 4641 * (temp3 - 1) / pow(x, 2.0);
	cout << 0.1 << " " << b / sqrt(N) << " " << b << "\n";
	
	x = res / 2;
	b = (1.0 / sqrt(2) + (3.0 / sqrt(2) - 2) * x) * temp1 +
		temp2 * (2.0 / x - 20.0 * x + 167.0 / 13 +
			12440.0 / 4641 * temp3 * pow(x, 3.0) + 424.0 / 221 / x +
			43468.0 / 4641 * temp3 * pow(x, 2.0)) + 56612.0 / 4641 * temp3 * x +
		33428.0 / 4641 * temp3 + 8692.0 / 4641 * temp3 / x +
		848 / 4641 * (temp3 - 1) / pow(x, 2.0);
	cout << res / 2 << " " << b / sqrt(N) << " " << b << "\n";
	cout << res << " " << min / sqrt(N) << " " << min << "\n";
	return res;
}
int main() {
	/*curve_a = 0;
	curve_b = 4;
	curve_p = 520318357701949;
	curve_n = 520318403271057;
	curve_x = 216189723581448;
	curve_y = 102268133210595;
	*/curve_a = 0;
	curve_b = 4;
	curve_p = 11783108327;
	curve_n = 11783108328;
	curve_x = 886986656;
	curve_y = 2098373573;
	/*
	curve_p = 2338201507;
	curve_n = 2338134573;
	curve_x = 54147032;
	curve_y = 767592089;/*
	curve_a = 0;
	curve_b = 5;
	curve_p = 932716151002095169;
	curve_n = 932716151369922171;
	curve_x = 568539741918453349;
	curve_y = 344422844402218034;*/
	int num = 1;
	curve curve_P(curve_x, curve_y);
	mul[0] = curve_P;
	
	for (int i = 1; i < 100; ++i) {
		mul[i] = mul[i - 1] + mul[i - 1];
	}
	double alpha = getalpha(curve_n);
	clock_t starttime, endtime;
	srand((unsigned)time(NULL));
	type res_n;
	curve curve_Q;
	double res = 0, restime = 0;
	for (int i = 0; i < num; ++i) {
		res_n = (type)((double)rand() / RAND_MAX * (double)curve_n);
		cout << res_n << "\n";
		curve_Q = getnum(res_n);
		starttime = clock();
		GSwalk walk(curve_P, curve_Q, alpha / 2);
		endtime = clock();
		res += walk.num / sqrt(curve_n / 20) / num;
		restime += (endtime - starttime) / num;
		cout << res_n << " " << walk.res_n << " " << (walk.res_n == res_n)
			 << " " << walk.num / sqrt(curve_n /20) << " " << (endtime - starttime) << "ms\n";
	}
	cout << res << " " << restime;
	/*
	type num1 = 0, num2 = 0, num3 = 0, num4 = 0, num5 = 0;
	starttime = clock();
	for (int i = 0; i < num; ++i) {
		res_n = (type)((double)rand() / RAND_MAX * (double)curve_n);
		curve_Q = getnum(res_n);
		GSwalk walk(curve_P, curve_Q, 1);
		if (walk.res_n != res_n) {
			cout << "false1\n";
			break;
		}
		num1 += walk.num;
	}
	endtime = clock();
	cout << num1 / num / sqrt(curve_n) << " " << (endtime - starttime) << "ms\n";
	starttime = clock();
	for (int i = 0; i < num; ++i) {
		res_n = (type)((double)rand() / RAND_MAX * (double)curve_n);
		curve_Q = getnum(res_n);
		GSwalk walk(curve_P, curve_Q, 0.5);
		if (walk.res_n != res_n) {
			cout << "false2\n";
			break;
		}
		num2 += walk.num;
	}
	endtime = clock();
	cout << num2 / num / sqrt(curve_n) << " " << (endtime - starttime) << "ms\n";

	starttime = clock();
	for (int i = 0; i < num; ++i) {
		res_n = (type)((double)rand() / RAND_MAX * (double)curve_n);
		curve_Q = getnum(res_n);
		GSwalk walk(curve_P, curve_Q, 0.1);
		if (walk.res_n != res_n) {
			cout << res_n << " " << walk.res_n << " " << walk.num << "\n";
			cout << "false3\n";
			break;
		}
		num3 += walk.num;
	}
	endtime = clock();
	cout << num3 / num / sqrt(curve_n) << " " << (endtime - starttime) << "ms\n";
	starttime = clock();
	for (int i = 0; i < num; ++i) {
		res_n = (type)((double)rand() / RAND_MAX * (double)curve_n);
		curve_Q = getnum(res_n);
		GSwalk walk(curve_P, curve_Q, alpha);
		if (walk.res_n != res_n) {
			cout << res_n << " " << walk.res_n << " " << walk.num << "\n";
			cout << "false4\n";
			break;
		}
		num4 += walk.num;
	}
	endtime = clock();
	cout << num4 / num / sqrt(curve_n) << " " << (endtime - starttime) << "ms\n";
	starttime = clock();
	for (int i = 0; i < num; ++i) {
		res_n = (type)((double)rand() / RAND_MAX * (double)curve_n);
		curve_Q = getnum(res_n);
		GSwalk walk(curve_P, curve_Q, alpha / 2.0);
		if (walk.res_n != res_n) {
			cout << res_n << " " << walk.res_n << " " << walk.num << "\n";
			cout << "false5\n";
			break;
		}
		num5 += walk.num;
	}
	endtime = clock();
	cout << num5 / num / sqrt(curve_n) << " " << (endtime - starttime) << "ms\n";
	*/
}