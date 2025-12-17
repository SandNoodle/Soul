#pragma once
namespace Soul::IR
{
	template <InstructionKind T>
	constexpr auto Instruction::is() const noexcept -> bool
	{
		return dynamic_cast<const T*>(this) != nullptr;
	}

	template <InstructionKind T>
	constexpr auto Instruction::as() const noexcept -> const T&
	{
		return dynamic_cast<const T&>(*this);
	}

	template <InstructionKind T>
	constexpr auto Instruction::as() noexcept -> T&
	{
		return dynamic_cast<T&>(*this);
	}

	constexpr auto Instruction::no_args() noexcept -> Arguments { return Arguments{ nullptr, nullptr }; }

	constexpr auto Instruction::single_arg(Instruction* arg) noexcept -> Arguments { return Arguments{ arg, nullptr }; }

	constexpr auto Instruction::two_args(Instruction* arg0, Instruction* arg1) noexcept -> Arguments
	{
		return Arguments{ arg0, arg1 };
	}

	constexpr Instruction::Instruction(Types::Type type, Arguments args) : type(std::move(type)), args(std::move(args))
	{
	}

	constexpr Unreachable::Unreachable() : Instruction(Types::Type{}, Instruction::no_args()) {}

	constexpr Noop::Noop() : Instruction(Types::Type{ Types::PrimitiveType::Kind::VOID }, Instruction::no_args()) {}

	constexpr Cast::Cast(Types::Type type, Instruction* arg)
		: Instruction(std::move(type), Instruction::single_arg(arg))
	{
	}

	constexpr Call::Call(Types::Type return_type, std::string identifier, std::vector<Instruction*> parameters)
		: Instruction(std::move(return_type), Instruction::no_args()),
		  identifier(std::move(identifier)),
		  parameters(std::move(parameters))
	{
	}

	constexpr Const::Const(Types::Type type, Value value)
		: Instruction(std::move(type), Instruction::no_args()), value(std::move(value))
	{
	}

	constexpr Jump::Jump(BasicBlock* target)
		: Instruction(Types::Type{ Types::PrimitiveType::Kind::VOID }, Instruction::no_args()), target(target)
	{
	}

	constexpr JumpIf::JumpIf(Instruction* condition, BasicBlock* then_block, BasicBlock* else_block)
		: Instruction(Types::PrimitiveType::Kind::VOID, Instruction::single_arg(condition)),
		  then_block(then_block),
		  else_block(else_block)
	{
	}

	constexpr Return::Return(Instruction* arg)
		: Instruction(Types::Type{ Types::PrimitiveType::Kind::VOID }, Instruction::single_arg(arg))
	{
	}

	constexpr StackSlot::StackSlot(Types::Type type) : Instruction(std::move(type), Instruction::no_args()) {}

	constexpr StackStore::StackStore(StackSlot* slot, Instruction* value)
		: Instruction(Types::Type{ Types::PrimitiveType::Kind::VOID }, Instruction::single_arg(value)), slot(slot)
	{
	}

	constexpr StackLoad::StackLoad(StackSlot* slot)
		: Instruction(Types::Type{ Types::PrimitiveType::Kind::VOID }, Instruction::no_args()), slot(slot)
	{
	}

	constexpr GetArgument::GetArgument(Types::Type type, Index index)
		: Instruction(std::move(type), Instruction::no_args()), index(index)
	{
	}

	constexpr Phi::Phi(Types::Type type) : Instruction(std::move(type), Instruction::no_args()) {}

	constexpr Upsilon::Upsilon(Instruction* value, Instruction* phi)
		: Instruction(Types::Type{ Types::PrimitiveType::Kind::VOID }, Instruction::single_arg(value)), phi(phi)
	{
	}

	constexpr Not::Not(Instruction* arg)
		: Instruction(Types::Type{ Types::PrimitiveType::Kind::BOOLEAN }, Instruction::single_arg(arg))
	{
	}
};  // namespace Soul::IR
