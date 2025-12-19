#include "AST/Visitors/ErrorCollector.h"

namespace Soul::AST::Visitors
{
	ErrorCollectorVisitor::ErrorCollectorVisitor(std::size_t max_depth) : _depth_current(0), _depth_max(max_depth) {}

	void ErrorCollectorVisitor::Accept(ASTNode::Reference node)
	{
		if (!node) {
			return;
		}

		if (++_depth_current > _depth_max) {
			return;
		}
		node->Accept(*this);
		_depth_current--;
	}

	bool ErrorCollectorVisitor::IsValid() const noexcept { return _errors.empty(); }

	const ErrorCollectorVisitor::Errors& ErrorCollectorVisitor::GetErrors() const noexcept { return _errors; }

	void ErrorCollectorVisitor::Visit(const ErrorNode& node)
	{
		if (_depth_current > _depth_max) {
			return;
		}
		_errors.emplace_back(std::make_pair(_depth_current, &node));
	}
}  // namespace Soul::AST::Visitors
