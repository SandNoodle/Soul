#include "AST/Visitors/ErrorCollector.h"

namespace Soul::AST::Visitors
{
	ErrorCollectorVisitor::ErrorCollectorVisitor(std::size_t max_depth)
		: _depth_current(0), _depth_max(max_depth), _errors()
	{
	}

	void ErrorCollectorVisitor::accept(ASTNode::Reference node)
	{
		if (!node) {
			return;
		}

		if (++_depth_current > _depth_max) {
			return;
		}
		node->accept(*this);
		_depth_current--;
	}

	bool ErrorCollectorVisitor::is_valid() const noexcept { return _errors.empty(); }

	const ErrorCollectorVisitor::Errors& ErrorCollectorVisitor::errors() const noexcept { return _errors; }

	void ErrorCollectorVisitor::visit(const ErrorNode& node)
	{
		if (_depth_current > _depth_max) {
			return;
		}
		_errors.emplace_back(std::make_pair(_depth_current, &node));
	}
}  // namespace Soul::AST::Visitors
