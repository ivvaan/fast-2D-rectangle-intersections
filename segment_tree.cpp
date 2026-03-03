/*
*
*      Copyright (c)  2026  Ivan Balaban
*      ivvaan@gmail.com
*

This file is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This file is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

For GNU General Public License terms and conditions
 see <http://www.gnu.org/licenses/>.
*/#include <iostream>
#include <cassert>
#include <vector>
#include <random>
#include <numeric>
#include <algorithm>
#include <chrono>

namespace SegmentTreeAndList {

  struct interval {
    int l = 0;
    int r = 0;
    interval() = default;
    interval(int _l, int _r) : l(_l), r(_r) {}
    interval(int _l) : l(_l), r(_l + 1) {}
    interval operator+(const interval& other) const {
      assert((r == other.l) || (l == other.r));
      return (r == other.l) ? interval{ l, other.r } : interval{ other.l, r };
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

  template<int P=1>
  struct TreeList {
    constexpr static const int S = (1 << P) - 1;
    constexpr static const int Q = S - 1;
    constexpr static const int M = ~S;
    int* tree_list = nullptr;//[0..sz-1] - used by binary tree to store filling subtree info, 
    //[sz..SZ-1] - list of filled elements, tree_list[i] is the next filled element in list after i, 
    // tree_list[0] is the head of list (the first filled element in list or 1 if list is empty)
    // if list is not empty, tree_list[i] is always >= 1 
    int n = 0;
    int sz = 0;//first power of 2 greater or equal to n
    int SZ = 0;//sz+n

    bool is_filled(int pos) const {
      return tree_list[pos];
    }

    static int get_rightmost_son(int father) {
      return 1 + (father << P);
    }

    static int get_leftmost_sibling(int pos) {
      return ((pos + Q) & M) - Q;
    }

    int get_left_filled_sibling(int pos) const {
      auto ls = get_leftmost_sibling(pos);
      --pos;
      while (ls <= pos) {
        if (is_filled(pos))
          return pos;
        --pos;
      }
      return 0;
    };

    int get_rightmost_filled_son(int father) const {
      int i = get_rightmost_son(father);
      while (!is_filled(i))
        --i;
      return i;
    };

    static int get_father(int pos) {
      return (pos+Q) >> P;
    }

    static constexpr const bool lst_del = false;
    static constexpr const bool lst_ins = true;

   

    template<bool is_insert>
    void list_change(int rank) {
      auto pos = sz + rank;
      int prev_elem=0;
      while (pos!=1) {
        if (prev_elem=get_left_filled_sibling(pos)) {
          pos = get_father(pos);
          break;
        }
        if constexpr (is_insert) {
          ++tree_list[pos = get_father(pos)];
        }
        else {
          --tree_list[pos = get_father(pos)];
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
          pos = get_father(pos);
        }
        while (prev_elem < sz) 
          prev_elem = get_rightmost_filled_son(prev_elem);
      }
      pos = sz + rank;

      if constexpr (is_insert) {
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
      return tree_list[sz + rank] - sz;
    }


    int get_sz() {
      int v = 1;
      int r = 1;
      while (v < n) {
        r += v;
        v <<= P;
      }
      return r;
    }

    TreeList(int _n) : n(_n) {
      sz = get_sz();
      SZ = sz + n;
      tree_list = new int[SZ];
      std::fill_n(tree_list, SZ, 0);
      tree_list[0] = 1;//fake last list element is stored in 0 position - the header of list
      //ordered_list[i]==1 where i>=sz means that i is filled, but next filled element is not exists (i.e.last element in list is i)
    }

    ~TreeList() {
     if (tree_list) {
        delete[] tree_list;
        tree_list = nullptr;
      }
    }

  };

  template<>
  int TreeList<1>::get_left_filled_sibling(int pos) const {
    if ((pos & 1) == 0) return 0;
    auto sibling = pos ^ 1;
    return is_filled(sibling) ? sibling : 0;
  }

  template<>
  int TreeList<1>::get_rightmost_filled_son(int pos) const {
    auto rs = get_rightmost_son(pos);
    return is_filled(rs) ? rs : rs - 1;
  }

  struct STree {
    interval* tree = nullptr;
    interval* levels = nullptr;
    int n = 0;
    int sz = 0;//first power of 2 greater or equal to n
    int SZ = 0;//sz+n
    int depth = 0;

    int get_tree_size() const {
      return SZ >> 1;
    }

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
      levels[lv] = { l,r };
      --r;
      do {
        tree[(r >> 1)] = tree[r];
        auto tree_ = tree + 1;
        for (int i = l; i < r; i += 2)
          tree[i >> 1] = tree[i] + tree_[i];
        --r;
        r = (r >> 1) + ((r - l) & 1);
        l = l >> 1;
        levels[--lv] = { l,r + 1 };
      } while ((l > 1) || (r > 1));
    }
    void print_insert(int pos, int id) {
      if (tree)
        std::cout << "insert " << id << " to " << tree[pos] << "\n";
      else
        std::cout << "insert " << id << " to " << pos << "\n";
    }

    template<typename action_func>
    void locate(int rank, int id, action_func do_locate) {
      auto pos = (sz + rank) >> 1;
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
      if (pow < 1) return;
      auto idx = (sz >> pow) + from;
      do_insert(idx, id);
      if (from + 1 < to)
        do_insert(idx + 1, id);
      if (pow < 2) return;
      insert_left(l, from << pow, id, do_insert);
      insert_right(to << pow, r, id, do_insert);
    }

    template<typename action_func>
    void insert_left(int l, int r, int id, action_func do_insert) {
      if (l + 2 > r)
        return;
      auto l_next = l + 1;
      do {
        auto pow = std::bit_width((unsigned)(r - l))-1;
        r -= (1 << pow);
        do_insert((sz + r) >> pow, id);
      } while (l_next < r);
    }

    template<typename action_func>
    void insert_right(int l, int r, int id, action_func do_insert) {
      if (l + 2 > r)
        return;
      auto r_prev = r - 1;
      do {
        auto pow = std::bit_width((unsigned)(r - l))-1;
        do_insert((sz + l) >> pow, id);
        l += (1 << pow);
      } while (l < r_prev);
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
      SZ = sz + n;
      depth = get_depth();
#ifdef _DEBUG
      construct();
#endif  
    }

    ~STree() {
      if (tree) {
        delete[] tree;
        tree = nullptr;
      }
      if (levels) {
        delete[] levels;
        levels = nullptr;
      }
    }

    void print(std::ostream& os, int l, int r) const {
      if (tree == nullptr)
        return;
      if ((l < 2) && (r < 3)) {
        os << tree[l] << "\n";
        return;
      }
      print(os, l >> 1, (r >> 1) + ((r - l) & 1));
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

}

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

  int *get_sorted_bounds(int *buf, bool axis) const {
    //precondition buf = new int[size() * 2]
    std::iota(buf, buf + size() * 2, 0);
    std::sort(buf, buf + size() * 2, [arr = ((double*)rects.data()) + axis](int a, int b) {
      return arr[a<<1] < arr[b<<1];
    });
    return buf;
  }
};

static bool rects_intersect(const drect& a, const drect& b) {
  return !(a.ru.x < b.ld.x || b.ru.x < a.ld.x || a.ru.y < b.ld.y || b.ru.y < a.ld.y);
}

template<typename action_func>
void rect_intersections_trivial(const rect_set& rs, action_func reporter) {
  int n = static_cast<int>(rs.size());
  for (int i = 0; i < n; ++i) {
    for (int j = i + 1; j < n; ++j) {
      if (rects_intersect(rs.rects[i], rs.rects[j]))
        reporter(i, j);
    }
  }
}

template<typename action_func>
void rect_intersections(const rect_set& rs, action_func reporter) {

  struct count_info {
    int ins = 0, del = 0, max = 0;
    void insert() { if (++ins > max) max = ins; }
    void erase() { ++del; }
    void locate() { ins -= del; del = 0; }
  };

  struct arr_info {
    int beg = 0, end = 0;
  };


  using namespace SegmentTreeAndList;
  int n = static_cast<int>(rs.size());

  auto ranks2pointsX_keeper = std::make_unique<int[]>(n * 2);
  auto ranks2pointsX = rs.get_sorted_bounds(ranks2pointsX_keeper.get(), rect_set::axis_X);
  auto ranks2pointsY_keeper = std::make_unique<int[]>(n * 2);
  auto ranks2pointsY = rs.get_sorted_bounds(ranks2pointsY_keeper.get(), rect_set::axis_Y);
  
  auto points2ranksY_keeper = std::make_unique<int[]>(n * 2);
  auto points2ranksY = points2ranksY_keeper.get();
  for (int i = 0; i < n * 2; ++i) {
    points2ranksY[ranks2pointsY[i]] = i;
  }
  auto ranks2rectanglesY = ranks2pointsY;//we dont need ranks2pointsY, but we need ranks2rectanglesY, so we can reuse allocated array
  for (int i = 0; i < n * 2; ++i) {
    ranks2rectanglesY[i] >>= 1;//rectangle id is any endpoint id divided by 2, because each rectangle has 2 endpoints
  }


  STree st(n * 2);
  TreeList<2> tl(n * 2);
  //first we need to calculate how many rectangles can be in each node of segment tree at the same time, to allocate arrays for nodes
  auto tree_size = st.get_tree_size();
  auto counts_keeper = std::make_unique<count_info[]>(tree_size);
  auto counts = counts_keeper.get();
  std::fill_n(counts, tree_size, count_info{});

  for (int i = 0; i < n * 2; ++i) {
    auto point = ranks2pointsX[i];
    auto rect_id = point >> 1;
    if ((point & 1) == 0) {
      st.locate(points2ranksY[rect_id << 1], rect_id, [=](int pos, int id) {
        assert(pos < tree_size);
        counts[pos].locate();
        });
      st.insert_range(points2ranksY[rect_id << 1], points2ranksY[(rect_id << 1) + 1], rect_id, [=](int pos, int id) {
        assert(pos < tree_size);
        counts[pos].insert();
        });
    }
    else {
      st.insert_range(points2ranksY[rect_id << 1], points2ranksY[(rect_id << 1) + 1], rect_id, [=](int pos, int id) {
        assert(pos < tree_size);
        counts[pos].erase();
        });
    }
  }
  //now we know the maximum count of rectangles in each node, we can allocate arrays for nodes
  auto node_arrays_keeper = std::make_unique<arr_info[]>(tree_size);
  auto node_arrays = node_arrays_keeper.get();
  int acc = 0;
  for (int i = 0; i < tree_size; ++i) {
    node_arrays[i].beg = node_arrays[i].end = acc;
    acc += counts[i].max;
  }
  counts_keeper.reset();
 // With per-node arrays allocated, run the main algorithm: sweep along the X axis.
 // For each rectangle, locate its lower Y endpoint in the segment tree to find
 // all rectangles that overlap it in Y. Then insert the rectangle for future checks
 // and scan the corner list between its Y endpoints, since those also intersect.
 // Duplicates can appear both in tree nodes and the corner list, so use
 // `dublicate_checker` to avoid reporting the same pair more than once.
  auto node_rects_keeper = std::make_unique<int[]>(acc);
  auto node_rects = node_rects_keeper.get();
  auto is_removed_keeper = std::make_unique<char[]>(n);
  auto is_removed = is_removed_keeper.get();
  std::fill_n(is_removed, n, 0);
  auto dublicate_checker_keeper = std::make_unique<int[]>(n);
  auto dublicate_checker = dublicate_checker_keeper.get();
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
          //lazy deletion - we mark rectangles as removed, but do not remove them from arrays, 
          // until we need to locate new rectangle, then we do remove all removed rectangles 
          // from array and update end index.
          if (is_removed[other])
            continue;
          node_rects[new_end++] = other;
          dublicate_checker[other] = rect;
          reporter(rect, other);
        }
        arr_info.end = new_end;
        };
      st.locate(beginY, rect_id, do_locate);

      auto do_insert = [=](int pos, int id) {
        node_rects[node_arrays[pos].end++] = id;
        };
      st.insert_range(beginY, endY, rect_id, do_insert);
      tl.list_insert(endY);
      tl.list_insert(beginY);
      auto next = tl.get_next(beginY);
      while (next != endY) {
        auto other_id = ranks2rectanglesY[next];
        next = tl.get_next(next);
        if (dublicate_checker[other_id] == rect_id)
          continue;
        dublicate_checker[other_id] = rect_id;
        reporter(rect_id, other_id);
      }
    }
    else {
      tl.list_delete(beginY);
      tl.list_delete(endY);
      //we do not remove rectangle from arrays, but mark it as removed, so it will be removed later when we need to locate new rectangle
      is_removed[rect_id] = 1;
    }
  }
}

int main()
{
  rect_set rs;
  int N = 256 * 256 + 1;
  rs.fill_random(N, 52);

  std::vector<std::pair<int, int>> fast;
  std::vector<std::pair<int, int>> trivial;

  auto collect = [](std::vector<std::pair<int, int>>& out) {
    return [&out](int a, int b) {
      if (a > b)
        std::swap(a, b);
      out.emplace_back(a, b);
      };
    };
  auto collect2 = [](long long& out) {
    return [&out](int a, int b) {
      ++out;
       };
    };

  long long fast_count = 0;
  long long trivial_count = 0;

  auto t0 = std::chrono::high_resolution_clock::now();
  rect_intersections(rs, collect2(fast_count));
  auto t1 = std::chrono::high_resolution_clock::now();
  //return 0;
  auto t2 = std::chrono::high_resolution_clock::now();
  rect_intersections_trivial(rs, collect2(trivial_count));
  auto t3 = std::chrono::high_resolution_clock::now();

  auto fast_ms = std::chrono::duration_cast<std::chrono::milliseconds>(t1 - t0).count();
  auto trivial_ms = std::chrono::duration_cast<std::chrono::milliseconds>(t3 - t2).count();

  std::cout << "rect_intersections: " << fast_ms << " ms\n";
  std::cout << "rect_intersections_trivial: " << trivial_ms << " ms\n";


  auto normalize = [](std::vector<std::pair<int, int>>& v) {
    std::sort(v.begin(), v.end());
    //v.erase(std::unique(v.begin(), v.end()), v.end());
  };

  //normalize(fast);
  //normalize(trivial);

  if (fast_count == trivial_count) {
    std::cout << "OK: intersections count = " << fast_count << "\n";
    std::cout << "Speedup: " << static_cast<double>(trivial_ms) / fast_ms << "x\n";
    std::cout << (double)trivial_count*450 / N / (N-1) << "% : percent of theortical value (4/9) intersections per rectangle\n";
  }
  else {
    std::cout << "Mismatch: fast=" << fast_count << " trivial=" << trivial_count << "\n";
  }

  return 0;
}

