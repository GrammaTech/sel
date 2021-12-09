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
    double segdist = p1->Distance(*p2);
    if ((d + segdist) > dist) {
      double frac = (dist - d) / segdist;
      auto midpoint = p1->PointAlongSegment(*p2, frac);
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
