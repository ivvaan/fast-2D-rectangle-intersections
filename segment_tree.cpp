// segment_tree.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <iostream>
#include <cassert>
#include <vector>
#include <random>
#include <numeric>
#include <algorithm>

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

struct count_info {
  int ins = 0;
  int del = 0;
  int max = 0;
  void insert() {
    ++ins;
    if (ins > max)
      max = ins;
  }
  void erase() {
    ++del;
  }
  void locate() {
    ins -= del;
    assert(ins >= 0); 
    del = 0;
  }
};


struct STree {
  interval* tree = nullptr;
  interval* levels = nullptr;
  int* tree_list = nullptr;//[0..sz-1] - used by binary tree to store filling subtree info, 
  //[sz..SZ-1] - list of filled elements, tree_list[i] is the next filled element in list after i, 
  // tree_list[0] is the head of list (the first filled element in list or 1 if list is empty)
  // if list is not empty, tree_list[i] is always >= sz 
  int n = 0;
  int sz = 0;//first power of 2 greater or equal to n
  int SZ = 0;//sz+n
  int depth = 0;

  void construct() {
    tree = new interval[SZ];
    auto t = tree + sz;
    for (int i = 0; i < n; ++i) {
      t[i] = interval(i);
    }
    levels = new interval[depth + 1];
    int l = sz;
    int r = SZ;
    int lv = depth;
    levels[lv] = {l,r};
    --r;
    do {
      tree[(r >> 1)] = tree[r];
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
    if(tree)
      std::cout << "insert " << id << " to " << tree[pos] << "\n";
    else
      std::cout << "insert " << id << " to " << pos << "\n";
  }

  template<typename action_func>
  void locate(int rank, int id, action_func do_locate) {
    auto pos = (sz+rank);
    do {
      do_locate(pos, id);
      pos >>= 1;
    } while (pos > 0);
  }

  template<typename action_func>
  void insert_range(int l, int r, int id, action_func do_insert) {
    if ((l == 0) && (r == n)) {
      do_insert(1, id);
      return;
    }
    int pow = depth;
    int from, to;
    do {
      --pow;
      from = (l >> pow);
      from += l != (from << pow);
      to = r >> pow;
    } while (from >= to);
    auto idx = (1 << (depth - pow)) + from;
    do_insert(idx, id);
    if (from + 1 < to)
      do_insert(idx + 1, id);
    if (pow != 0) {
      insert_left(pow - 1, l, from << pow, id, do_insert);
      insert_right(pow - 1, to << pow, r, id, do_insert);
    }
  }

  template<typename action_func>
  void insert_left(int pow, int l, int r, int id, action_func do_insert) {
    if(l==r)
      return;
    do {
      for (auto d = r - l; (d >> pow) == 0; --pow);
      r -= (1 << pow);
      do_insert((sz + r) >> pow, id);
    } while ((pow != 0) && (l != r));
  }

  template<typename action_func>
  void insert_right(int pow, int l, int r, int id, action_func do_insert) {
    if (l == r)
      return;
    do {
      for (auto d = r - l; (d >> pow) == 0; --pow);
      do_insert((sz + l) >> pow, id);
      l += (1 << pow);
    } while ((pow != 0) && (l != r));
  }


  bool is_filled(int pos) const {
    return tree_list[pos];
  }
  static int get_sibling(int pos) {
    return pos ^ 1;
  }
  static bool is_right_son(int pos) {
    return pos & 1;
  }

  static int get_left_son(int father) {
    return father << 1;
  }
  static int get_right_son(int father) {
    return (father << 1) + 1;
  }

  static constexpr const bool lst_del = false;
  static constexpr const bool lst_ins = true;

  template<bool is_insert>
  void list_change(int rank) {
    auto pos = sz + rank;
    int prev_elem = 0;
    while (pos!=1) {
      if (is_right_son(pos) //comes from right
        && is_filled(get_sibling(pos))//left brother is filled
        ){
        prev_elem = get_sibling(pos);
        pos >>= 1;
        break;
      }
      if constexpr (is_insert) {
        ++tree_list[pos >>= 1];
      }
      else {
        --tree_list[pos >>= 1];
        assert(tree_list[pos] >= 0);
      }
    };
    if (prev_elem) {
      while (pos) {
        if constexpr (is_insert) {
          ++tree_list[pos];
        }
        else {
          --tree_list[pos];
          assert(tree_list[pos] >= 0);
        }
        pos >>= 1;
      }
      while(prev_elem<sz) {
        auto rs = get_right_son(prev_elem);
        prev_elem= is_filled(rs) ? rs : rs - 1;
      }
    }
    pos = sz + rank;

    if constexpr(is_insert) {
      tree_list[pos] = tree_list[prev_elem];
      tree_list[prev_elem] = pos;
    }
    else {
      assert(tree_list[prev_elem] == pos);
      tree_list[prev_elem] = tree_list[pos];
      tree_list[pos] = 0;
    }
  }

  void list_insert(int rank) {
    list_change<lst_ins>(rank);
  }

  void list_delete(int rank) {
    list_change<lst_del>(rank);
  }

  int get_next(int rank) const {
    return tree_list[sz + rank]-sz;
  }

  int get_sz() {
    int v = 1;
    while (v < n) v <<= 1;
    return v;
  }

  int get_depth() {
    int d = 0;
    for (int v = n; v > 0; v >>= 1)
      ++d;
    return d;
  }

  STree(int _n) : n(_n) {
    sz = get_sz();
    SZ =sz + n;
    depth = get_depth();
    tree_list = new int[SZ];
    std::fill_n(tree_list, SZ, 0);
    tree_list[0] = 1;//fake last list element is stored in 0 position - the header of list
    //ordered_list[i]==1 where i>=sz means that i is filled, but next filled element is not exists (i.e.last element in list is i)

#ifdef _DEBUG
    construct();
#endif  
  }

  ~STree() {
    if (tree) {
      delete[] tree;
      tree = nullptr;
    }
    if (levels){
      delete[] levels;
      levels = nullptr;
    }
    if(tree_list) {
      delete[] tree_list;
      tree_list = nullptr;
    }
  }

  void print(std::ostream& os,int l,int r) const {
    if(tree == nullptr)
      return;
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

struct dpoint {
  double x = 0, y = 0;
};

struct drect {
  dpoint ld, ru;
};

struct rect_set {
  std::vector<drect> rects;
  void fill_random(int n, int seed = 1) {
    rects.reserve(n);
    std::mt19937_64 rng;
    if (seed == 0) {
      std::random_device rd;
      rng.seed(rd());
    }
    else {
      rng.seed(static_cast<std::mt19937_64::result_type>(seed));
    }
    std::uniform_real_distribution<double> dist(0.0, 1.0);

    for (int i = 0; i < n; ++i) {
      dpoint ld = { dist(rng), dist(rng) };
      dpoint ru = { dist(rng), dist(rng) };
      if (ld.x > ru.x)
        std::swap(ld.x, ru.x);
      if (ld.y > ru.y)
        std::swap(ld.y, ru.y);
      rects.push_back({ ld, ru });
    }
  }
  auto size() const {
    return rects.size();
  }
  static constexpr const bool axis_X = false;
  static constexpr const bool axis_Y = true;

  int *get_sorted_bounds(int *buf, int* p_ranks,bool axis) const {
    //precondition buf = new int[size() * 2], p_ranks = new int[size() * 2]
    std::iota(buf, buf + size() * 2, 0);
    double* arr = ((double*)rects.data()) + axis;
    std::sort(buf, buf + size() * 2, [arr](int a, int b) {
      return arr[a<<1] < arr[b<<1];
    });
    for(int i = 0; i < size() * 2; ++i) {
      p_ranks[buf[i]] = i;
    }
    return buf;
  }
};

struct arr_info {
  int beg = 0;
  int end = 0;
};

template<typename action_func>
void rect_intersections(const rect_set& rs, action_func reporter) {
  int n = static_cast<int>(rs.size());
  int* ranks2pointsX = new int[n * 2];
  int* points2ranksX = new int[n * 2];
  rs.get_sorted_bounds(ranks2pointsX, points2ranksX, rect_set::axis_X);
  int* ranks2pointsY = new int[n * 2];
  int* points2ranksY = new int[n * 2];
  rs.get_sorted_bounds(ranks2pointsY, points2ranksY, rect_set::axis_Y);
  STree st(n * 2);
  count_info* counts = new count_info[st.SZ];
  memset(counts, 0, st.SZ * sizeof(count_info));
  for (int i = 0; i < n * 2; ++i) {
      auto point = ranks2pointsX[i];
      auto rect_id = point >> 1;
      if ((point & 1) == 0) {
        st.locate(points2ranksY[rect_id << 1], rect_id, [counts](int pos, int id) {
          counts[pos].locate();
          });
        st.insert_range(points2ranksY[rect_id << 1], points2ranksY[(rect_id << 1) + 1], rect_id, [counts](int pos, int id) {
          counts[pos].insert();
        });
      }
      else {
        st.insert_range(points2ranksY[rect_id << 1], points2ranksY[(rect_id << 1) + 1], rect_id, [counts](int pos, int id) {
          counts[pos].erase();
          });
      }
  }
  arr_info* node_arrays = new arr_info[st.SZ];
  int acc = 0;
  for (int i = 0; i < st.SZ; ++i) {
    node_arrays[i].beg = node_arrays[i].end = acc;
    acc += counts[i].max;
  }
  delete[] counts;
  int* node_rects = new int[acc];
  char* is_removed = new char[n];
  std::fill_n(is_removed, n, 0);
  int* dublicate_checker = new int[n];
  std::fill_n(dublicate_checker, n, -1);

  for (int i = 0; i < n * 2; ++i) {
    auto point = ranks2pointsX[i];
    auto rect_id = point >> 1;
    auto beginY = points2ranksY[rect_id << 1];
    auto endY = points2ranksY[point | 1];

    if ((point & 1) == 0) {

      auto do_locate = [=](int pos, int rect) {
        auto& arr_info = node_arrays[pos];
        int new_end = arr_info.beg;
        for (int i = arr_info.beg; i < arr_info.end; ++i) {
          auto other = node_rects[i];
          if (is_removed[other])
            continue;
          node_rects[new_end++] = other;
          //if (dublicate_checker[other] == rect) continue;//should not happen at this stage, but just in case
          dublicate_checker[other] = rect;
          //report intersection between rect and other !!!
          reporter(rect, other);
        }
        arr_info.end = new_end;
        };
      st.locate(beginY, rect_id,do_locate);

      auto do_insert = [=](int pos, int id) {
        node_rects[node_arrays[pos].end++] = id;
        };
      st.insert_range(beginY, endY, rect_id, do_insert);
      st.list_insert(endY);
      st.list_insert(beginY);
      auto next = st.get_next(beginY);
      while(next != endY) {
        auto other_id = ranks2pointsY[next] >> 1;
        next=st.get_next(next);
        if (dublicate_checker[other_id] == rect_id) //can be dublicates
          continue;
        dublicate_checker[other_id] = rect_id;
        //report intersection between rect_id and other_id !!!
        reporter(rect_id, other_id);
      }
    }
    else {
      st.list_delete(beginY);
      st.list_delete(endY);
      is_removed[rect_id] = 1;
    }
  }

}

int main()
{
  return 0;
}

