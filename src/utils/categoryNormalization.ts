// ─────────────────────────────────────────────────────────────
// Category name normalization & smart duplicate detection
// Used by both the check-duplicate API and the /admin/node route
// ─────────────────────────────────────────────────────────────

/**
 * Prefixes / suffixes / filler words that are removed before comparison.
 * e.g.  "Best Restaurants in Mumbai" → "restaurants"
 *       "Top Hotels Near Me"          → "hotels"
 *       "IT Services & Solutions"     → "it"
 */
export const STRIP_WORDS = new Set([
  // Superlatives / qualifiers
  "best", "top", "great", "good", "cheap", "affordable", "premium",
  "trusted", "famous", "popular", "leading", "certified",
  // Location suffixes
  "near", "me", "nearby", "around", "local", "online", "in", "at",
  "city", "town", "india", "mumbai", "delhi", "bangalore", "pune",
  // Service suffix words
  "services", "service", "solutions", "solution", "providers", "provider",
  "experts", "expert", "professionals", "professional",
  "agents", "agent", "dealers", "dealer",
  "shops", "shop", "stores", "store",
  "centres", "centre", "centers", "center",
  "specialists", "specialist", "companies", "company",
  "institute", "institutes", "academy", "class", "classes",
  // Articles & conjunctions
  "and", "or", "the", "a", "an", "of", "with", "by", "for", "to",
]);

const SPECIAL_CHAR_RE = /[&/\\|\-_.,;:!?()'"+@#%*[\]{}<>]+/g;

// ──────────────────────────────────────────────────────────────
// Core normalization

/**
 * Full normalization used for storage / comparison.
 * "Best Restaurants in Mumbai" → "restaurants"
 */
export function normalizeCategory(name: string): string {
  return name
    .toLowerCase()
    .replace(SPECIAL_CHAR_RE, " ")  // special chars → spaces
    .split(" ")
    .map((w) => w.trim())
    .filter((w) => w.length > 1 && !STRIP_WORDS.has(w))
    .join(" ")
    .trim();
}

/**
 * Human-readable normalized preview for the admin UI.
 * Capitalises first letter of each word.
 * "top restaurants near me" → "Restaurants"
 */
export function normalizedPreview(name: string): string {
  const core = normalizeCategory(name);
  return core
    .split(" ")
    .map((w) => w.charAt(0).toUpperCase() + w.slice(1))
    .join(" ");
}

// ──────────────────────────────────────────────────────────────
// Similarity helpers

/** Jaccard similarity on word sets (0 → 1) */
function jaccardSimilarity(a: string, b: string): number {
  const wa = new Set(a.split(" ").filter(Boolean));
  const wb = new Set(b.split(" ").filter(Boolean));
  if (wa.size === 0 || wb.size === 0) return 0;
  const intersection = [...wa].filter((w) => wb.has(w)).length;
  const union = new Set([...wa, ...wb]).size;
  return intersection / union;
}

/** Levenshtein distance */
function levenshtein(a: string, b: string): number {
  const m = a.length;
  const n = b.length;
  if (m === 0) return n;
  if (n === 0) return m;
  const prev = Array.from({ length: n + 1 }, (_, i) => i);
  const curr = new Array<number>(n + 1);
  for (let i = 1; i <= m; i++) {
    curr[0] = i;
    for (let j = 1; j <= n; j++) {
      const cost = a[i - 1] === b[j - 1] ? 0 : 1;
      curr[j] = Math.min(curr[j - 1] + 1, prev[j] + 1, prev[j - 1] + cost);
    }
    prev.splice(0, prev.length, ...curr);
  }
  return prev[n];
}

// ──────────────────────────────────────────────────────────────
// Duplicate detection types & logic

export type MatchType = "exact" | "case-insensitive" | "normalized-exact" | "similar";

export interface DuplicateMatch {
  _id: string;
  name: string;
  icon: string;
  level: number;
  parent_id: string | null;
  matchType: MatchType;
  /** Similarity score 0–1 */
  similarity: number;
  /** The normalized form of the candidate's name */
  normalizedName: string;
}

export interface DuplicateCheckResult {
  normalizedInput: string;
  previewName: string;
  isDuplicate: boolean;         // true if exact or case-insensitive match
  hasSimilar: boolean;          // true if fuzzy-similar matches found
  hasWarning: boolean;          // isDuplicate || hasSimilar
  exactMatch: DuplicateMatch | null;
  similarMatches: DuplicateMatch[];
}

/**
 * Find exact / case-insensitive / similar matches for `inputName`
 * against `candidates` (all sibling categories at the same level).
 */
export function findDuplicates(
  inputName: string,
  candidates: Array<{
    _id: any; // accepts string | mongoose.Types.ObjectId
    name: string;
    icon: string;
    level: number;
    parent_id?: any | null;
  }>
): DuplicateCheckResult {
  const normInput = normalizeCategory(inputName);
  const preview  = normalizedPreview(inputName);

  const exactMatches: DuplicateMatch[] = [];
  const similar:     DuplicateMatch[] = [];

  for (const c of candidates) {
    const normCandidate = normalizeCategory(c.name);

    // 1. Exact string match
    if (c.name === inputName) {
      exactMatches.push({
        _id: String(c._id),
        name: c.name,
        icon: c.icon,
        level: c.level ?? 0,
        parent_id: c.parent_id ? String(c.parent_id) : null,
        matchType: "exact",
        similarity: 1,
        normalizedName: normCandidate,
      });
      continue;
    }

    // 2. Case-insensitive
    if (c.name.toLowerCase() === inputName.toLowerCase()) {
      exactMatches.push({
        _id: String(c._id),
        name: c.name,
        icon: c.icon,
        level: c.level ?? 0,
        parent_id: c.parent_id ? String(c.parent_id) : null,
        matchType: "case-insensitive",
        similarity: 0.99,
        normalizedName: normCandidate,
      });
      continue;
    }

    // Skip empty normalised names
    if (!normInput || !normCandidate) continue;

    // 3. Normalised exact match (e.g. "Best Restaurants" == "Restaurants")
    if (normInput === normCandidate) {
      similar.push({
        _id: String(c._id),
        name: c.name,
        icon: c.icon,
        level: c.level ?? 0,
        parent_id: c.parent_id ? String(c.parent_id) : null,
        matchType: "normalized-exact",
        similarity: 0.95,
        normalizedName: normCandidate,
      });
      continue;
    }

    // 4. Substring containment
    if (
      normInput.includes(normCandidate) ||
      normCandidate.includes(normInput)
    ) {
      const sim =
        Math.min(normInput.length, normCandidate.length) /
        Math.max(normInput.length, normCandidate.length);
      if (sim >= 0.5) {
        similar.push({
          _id: String(c._id),
          name: c.name,
          icon: c.icon,
          level: c.level ?? 0,
          parent_id: c.parent_id ? String(c.parent_id) : null,
          matchType: "similar",
          similarity: sim,
          normalizedName: normCandidate,
        });
        continue;
      }
    }

    // 5. Jaccard word-set similarity ≥ 55 %
    const jaccard = jaccardSimilarity(normInput, normCandidate);
    if (jaccard >= 0.55) {
      similar.push({
        _id: String(c._id),
        name: c.name,
        icon: c.icon,
        level: c.level ?? 0,
        parent_id: c.parent_id ? String(c.parent_id) : null,
        matchType: "similar",
        similarity: jaccard,
        normalizedName: normCandidate,
      });
      continue;
    }

    // 6. Levenshtein for short names (≤ 25 chars after normalization)
    if (normInput.length <= 25 && normCandidate.length <= 25) {
      const dist = levenshtein(normInput, normCandidate);
      const maxLen = Math.max(normInput.length, normCandidate.length);
      const sim = 1 - dist / maxLen;
      if (sim >= 0.75) {
        similar.push({
          _id: String(c._id),
          name: c.name,
          icon: c.icon,
          level: c.level ?? 0,
          parent_id: c.parent_id ? String(c.parent_id) : null,
          matchType: "similar",
          similarity: sim,
          normalizedName: normCandidate,
        });
      }
    }
  }

  // Deduplicate by _id, sort by similarity desc
  const dedup = (arr: DuplicateMatch[]) =>
    arr
      .sort((a, b) => b.similarity - a.similarity)
      .filter((v, i, self) => self.findIndex((x) => x._id === v._id) === i);

  const dedupedExact  = dedup(exactMatches);
  const dedupedSimilar = dedup(similar);

  return {
    normalizedInput: normInput,
    previewName:     preview,
    isDuplicate:     dedupedExact.length > 0,
    hasSimilar:      dedupedSimilar.length > 0,
    hasWarning:      dedupedExact.length > 0 || dedupedSimilar.length > 0,
    exactMatch:      dedupedExact[0] ?? null,
    similarMatches:  dedupedSimilar.slice(0, 5), // max 5 suggestions
  };
}
