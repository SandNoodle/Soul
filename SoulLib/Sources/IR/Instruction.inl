#pragma once
namespace Soul::IR
{
	template <InstructionKind T>
	constexpr auto Instruction::Is() const noexcept -> bool
	{
		return dynamic_cast<const T*>(this) != nullptr;
	}

	template <InstructionKind T>
	constexpr auto Instruction::As() const noexcept -> const T&
	{
		return dynamic_cast<const T&>(*this);
	}

	template <InstructionKind T>
	constexpr auto Instruction::As() noexcept -> T&
	{
		return dynamic_cast<T&>(*this);
	}

	constexpr auto Instruction::NoArguments() noexcept -> Arguments { return Arguments{ nullptr, nullptr }; }

	constexpr auto Instruction::SingleArgument(Instruction* arg) noexcept -> Arguments
	{
		return Arguments{ arg, nullptr };
	}

	constexpr auto Instruction::TwoArguments(Instruction* arg0, Instruction* arg1) noexcept -> Arguments
	{
		return Arguments{ arg0, arg1 };
	}

	constexpr Instruction::Instruction(TypeIndex type_index, Arguments args)
		: type_index(type_index), args(std::move(args))
	{
	}

	constexpr Unreachable::Unreachable() : Instruction(types::Type{}, Instruction::NoArguments()) {}

	constexpr Noop::Noop() : Instruction(types::Type{ types::PrimitiveType::Kind::Void }, Instruction::NoArguments()) {}

	constexpr Cast::Cast(types::Type type, Instruction* arg)
		: Instruction(std::move(type), Instruction::SingleArgument(arg))
	{
	}

	constexpr Call::Call(types::Type return_type, std::string identifier, std::vector<Instruction*> parameters)
		: Instruction(std::move(return_type), Instruction::NoArguments()),
		  identifier(std::move(identifier)),
		  parameters(std::move(parameters))
	{
	}

	constexpr Const::Const(types::Type type, Value value)
		: Instruction(std::move(type), Instruction::NoArguments()), value(std::move(value))
	{
	}

	constexpr Jump::Jump(BasicBlock* target)
		: Instruction(types::Type{ types::PrimitiveType::Kind::Void }, Instruction::NoArguments()), target(target)
	{
	}

	constexpr JumpIf::JumpIf(Instruction* condition, BasicBlock* then_block, BasicBlock* else_block)
		: Instruction(types::PrimitiveType::Kind::Void, Instruction::SingleArgument(condition)),
		  then_block(then_block),
		  else_block(else_block)
	{
	}

	constexpr Return::Return(Instruction* arg)
		: Instruction(types::Type{ types::PrimitiveType::Kind::Void }, Instruction::SingleArgument(arg))
	{
	}

	constexpr StackSlot::StackSlot(types::Type type) : Instruction(std::move(type), Instruction::NoArguments()) {}

	constexpr StackStore::StackStore(StackSlot* slot, Instruction* value)
		: Instruction(types::Type{ types::PrimitiveType::Kind::Void }, Instruction::SingleArgument(value)), slot(slot)
	{
	}

	constexpr StackLoad::StackLoad(StackSlot* slot)
		: Instruction(types::Type{ types::PrimitiveType::Kind::Void }, Instruction::NoArguments()), slot(slot)
	{
	}

	constexpr GetArgument::GetArgument(types::Type type, Index index)
		: Instruction(std::move(type), Instruction::NoArguments()), index(index)
	{
	}

	constexpr Phi::Phi(types::Type type) : Instruction(std::move(type), Instruction::NoArguments()) {}

	constexpr Upsilon::Upsilon(Instruction* value, Instruction* phi)
		: Instruction(types::Type{ types::PrimitiveType::Kind::Void }, Instruction::SingleArgument(value)), phi(phi)
	{
	}

	constexpr Not::Not(Instruction* arg)
		: Instruction(types::Type{ types::PrimitiveType::Kind::Boolean }, Instruction::SingleArgument(arg))
	{
	}
};  // namespace Soul::IR
