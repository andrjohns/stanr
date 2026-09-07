#include <stanli/message_sink.hpp>

#include <cstdio>
#include <mutex>
#include <utility>

namespace stanli {

namespace {

std::mutex& sink_mutex() {
  static std::mutex m;
  return m;
}

MessageSink& current_sink() {
  static MessageSink s;
  return s;
}

std::mutex& diagnostic_mutex() {
  static std::mutex m;
  return m;
}

MessageSink& current_diagnostic_sink() {
  static MessageSink s;
  return s;
}

}  // namespace

void set_message_sink(MessageSink s) {
  std::lock_guard<std::mutex> lock(sink_mutex());
  current_sink() = std::move(s);
}

void emit_message(const std::string& text) {
  std::lock_guard<std::mutex> lock(sink_mutex());
  if (current_sink()) {
    current_sink()(text.data(), text.size());
    return;
  }
#ifndef STANLI_NO_STDIO
  // The default, and what both paths did before there was a sink: the
  // line and its newline to stdout. One fwrite per call rather than two,
  // so a line cannot be split by another thread's output.
  std::string line = text;
  line += '\n';
  std::fwrite(line.data(), 1, line.size(), stdout);
#endif
}

void set_diagnostic_sink(MessageSink s) {
  std::lock_guard<std::mutex> lock(diagnostic_mutex());
  current_diagnostic_sink() = std::move(s);
}

void emit_diagnostic(const std::string& text) {
  std::lock_guard<std::mutex> lock(diagnostic_mutex());
  if (current_diagnostic_sink()) {
    current_diagnostic_sink()(text.data(), text.size());
    return;
  }
#ifndef STANLI_NO_STDIO
  std::string line = text;
  line += '\n';
  std::fwrite(line.data(), 1, line.size(), stderr);
#endif
}

}  // namespace stanli
