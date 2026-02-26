// segment_tree.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <iostream>
#include <cassert>
struct interval {
  int l = 0;
  int r = 0;
  interval() = default;
  interval(int _l, int _r) : l(_l), r(_r) {}
  interval(int _l) : l(_l), r(_l+1) {}
  interval operator+( const interval& other) const {
    assert((r == other.l)||(l==other.r));
    return (r == other.l) ? interval{l, other.r} : interval{other.l, r};
  }

  friend std::ostream& operator <<(std::ostream& os, const interval& i) {
    os << "[" << i.l << ", " << i.r << ")";
    return os;
  }
};

struct level {
  int l = 0;
  int r = 0;
  int sz = 0;
};


struct STree {
  using ins_func = void (STree::*)(int, int);
  interval* tree = nullptr;
  interval* levels = nullptr;
  int n = 0;
  int sz = 0;
  int SZ = 0;
  int depth = 0;

  void construct(int l, int r) {
    int lv = depth;
    levels[--lv] = {l,r};
    --r;
    do {
      tree[r >> 1] = tree[r];
      auto tree_ = tree + 1;
      for (int i = l; i < r; i += 2)
        tree[i >> 1] = tree[i] + tree_[i];
      --r;
      r = (r >> 1) + ((r - l) & 1);
      l = l >> 1;
      levels[--lv] = { l,r + 1};
    } while ((l > 1) || (r > 1));
    assert(lv == 0);
  }
  void print_insert(int pos, int id) {
    std::cout << "insert " << id << " to " << tree[pos] << "\n";
  }

  void insert(int l, int r, int id, ins_func do_insert) {
    if ((l == 0) && (r == n)) {
      (this->*do_insert)(1, id);
      return;
    }
    insert_rec(l, r, id, do_insert);
  }

  void insert_left(int pow, int l, int r, int id, ins_func do_insert) {
    if(l==r)
      return;
    auto shift = 1 << (depth-1);
    do {
      for (auto d = r - l; (d >> pow) == 0; --pow);
      r -= (1 << pow);
      (this->*do_insert)((shift + r) >> pow, id);
    } while ((pow != 0) && (l != r));
  }

  void insert_right(int pow, int l, int r, int id, ins_func do_insert) {
    if (l == r)
      return;
    auto shift = 1 << (depth - 1);
    do {
      for (auto d = r - l; (d >> pow) == 0; --pow);
      (this->*do_insert)((shift + l) >> pow, id);
      l += (1 << pow);
    } while ((pow != 0) && (l != r));
  }



  void insert_rec(int l, int r, int id, ins_func do_insert) {
    int pow = (depth - 1);
    int from, to;
    do {
      --pow;
      from = (l >> pow);
      from += l != (from << pow);
      to = r >> pow;
    } while (from >= to);
    auto idx = (1 << (depth - 1 - pow)) + from;
    (this->*do_insert)(idx, id);
    if(from+1<to)
      (this->*do_insert)(idx + 1, id);
    if (pow != 0) {
      insert_left(pow -1, l, from<<pow, id, do_insert);
      insert_right(pow -1, to<<pow, r, id, do_insert);
    }
  }

  int getSZ() {
    int v = 1;
    while (v < n) v <<= 1;
    return v + n;
  }

  int get_depth() {
    int d = 0;
    for (int v = n; v > 0; v >>= 1) 
      ++d;
    return d+1;
  }

  STree(int _n) : n(_n) {
    SZ = getSZ();
    tree = new interval[SZ];
    sz =SZ - n;
    auto t = tree + sz;
    for (int i = 0; i < n; ++i) {
      t[i] = interval(i);
    }
    depth = get_depth();
    levels = new interval[depth];
    construct(sz, SZ);
  }

  ~STree(){
    delete[] tree;
    delete[] levels;
  }

  void print(std::ostream& os,int l,int r) const {
    if ((l < 2) && (r < 3)) {
      os << tree[l] << "\n";
      return;
    }
    print(os, l>>1, (r>>1)+((r-l)&1));
    for (int i = l; i < r; ++i) {
      os << tree[i] << " ";
    }
    os << "\n";
  }

  friend std::ostream& operator <<(std::ostream& os, const STree& st) {
    //for (int i = 0; i < st.SZ; ++i)     os << st.tree[i] << " ";
    st.print(os, st.sz, st.SZ);

    os << "\n";

    return os;
  }
};

int main()
{
  STree st(19);
    std::cout <<st<< "\n";
    //getchar();
    st.insert(3, 15, 121, &STree::print_insert);
}

// Run program: Ctrl + F5 or Debug > Start Without Debugging menu
// Debug program: F5 or Debug > Start Debugging menu

// Tips for Getting Started: 
//   1. Use the Solution Explorer window to add/manage files
//   2. Use the Team Explorer window to connect to source control
//   3. Use the Output window to see build output and other messages
//   4. Use the Error List window to view errors
//   5. Go to Project > Add New Item to create new code files, or Project > Add Existing Item to add existing code files to the project
//   6. In the future, to open this project again, go to File > Open > Project and select the .sln file
