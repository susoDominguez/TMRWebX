// Shared helpers for route modules
const ID_REGEX = /^[a-zA-Z0-9_-]+$/;

function isValidId(id) {
  return typeof id === "string" && ID_REGEX.test(id.trim());
}

function parseIdsInput(input) {
  if (!input) return [];
  if (typeof input === "string") {
    return input
      .split(",")
      .map((s) => s.trim())
      .filter((s) => s.length > 0);
  }
  if (Array.isArray(input))
    return input.map((s) => String(s).trim()).filter((s) => s.length > 0);
  return [];
}

function escapeQuotes(s) {
  return String(s).replace(/"/g, '\\"');
}

module.exports = { isValidId, parseIdsInput, escapeQuotes };
