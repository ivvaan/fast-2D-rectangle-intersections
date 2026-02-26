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
  void do_insert(int pos, int id) {
    std::cout << "insert " << id << " to " << tree[pos] << "\n";
  }
  void insert(int l, int r, int id) {
    if ((l == 0) && (r == n)) {
      do_insert(1, id);
      return;
    }
    insert_rec(1, l, r, id);
  }

  void insert_left(int lv, int l, int r, int id) {
    auto pow = (depth - lv - 1);
    auto from = (l >> pow);
    from += l != from << pow;
    auto to = r >> pow;
    if (from != to) {
      assert(from + 1 == to);
      assert((1 << lv) == levels[lv].l);
      do_insert((1<<lv) + from, id);
      from <<= pow;
      if ((pow != 0)&&(l != from))
        insert_left(lv + 1, l, from, id);
      return;
    }
    if (pow == 0)
      return;
    insert_left(lv + 1, l, r, id);
  }

  void insert_right(int lv, int l, int r, int id) {
    auto pow = (depth - lv - 1);
    auto from = l >> pow;
    auto to = r >> pow;
    if (from != to) {
      assert(from + 1 == to);
      assert((1 << lv) == levels[lv].l);
      do_insert((1<<lv) + from, id);
      if (pow == 0)
        return;
      to <<= pow;
      if (r != to)
        insert_right(lv + 1, to, r, id);
      return;
    }
    if (pow == 0)
      return;
    insert_right(lv + 1, l, r, id);
  }



  void insert_rec(int lv,int l, int r, int id) {
    auto pow = (depth - lv - 1);
    auto from = (l >> pow);
    from += l != (from<<pow);
    auto to = r >> pow;
    if (from < to) {
      auto b = levels[lv].l;
      assert((1 << lv) == b);

      for (int i = from; i < to; ++i)
        do_insert((1 << lv) + i, id);
      if(pow==0)
        return;
      from <<= pow;
      if(l!=from)
        insert_left(lv+1, l, from, id);
      to <<= pow;
      if(r!=to)
        insert_right(lv+1, to, r, id);
      return;
    }
    if (pow == 0)
      return;
    insert_rec(lv + 1, l, r, id);
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
    st.insert(7, 9, 1);
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
