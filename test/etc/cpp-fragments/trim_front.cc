// Adapted from midgard/utils.h in Valhalla (MIT license).

// The MIT License (MIT)
//
// Copyright (c) 2018 Valhalla contributors
// Copyright (c) 2015-2017 Mapillary AB, Mapzen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

// Trim the front of a polyline (represented as a list or vector of Point2).
// Returns the trimmed portion of the polyline. The supplied polyline is
// altered (the trimmed part is removed).
std::list<Point> trim_front(std::list<Point>& pts, const float dist) {
  // Return if less than 2 points
  if (pts.size() < 2) {
    return {};
  }

  // Walk the polyline and accumulate length until it exceeds dist
  std::list<Point> result;
  result.push_back(pts.front());
  double d = 0.0f;
  for (auto p1 = pts.begin(), p2 = std::next(pts.begin()); p2 != pts.end(); ++p1, ++p2) {
    Point& next_point = *p2;
    double segdist = p1->Distance(next_point);
    if ((d + segdist) > dist) {
      double frac = (dist - d) / segdist;
      auto midpoint = p1->PointAlongSegment(next_point, frac);
      result.push_back(midpoint);

      // Remove used part of polyline
      pts.erase(pts.begin(), p1);
      pts.front() = midpoint;
      return result;
    } else {
      d += segdist;
      result.push_back(*p2);
    }
  }

  // Used all of the polyline without exceeding dist
  pts.clear();
  return result;
}
