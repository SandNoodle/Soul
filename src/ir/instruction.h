#pragma once

#include "common/types/type.h"
#include "common/value.h"
#include "core/types.h"
#include "ir/instruction_fwd.h"

#include <array>

namespace soul::ir
{
	/**
	 * @brief Represents a single Instruction in the language's Intermediate Representation.
	 * @detials Represented in Static Single-Assignment (SSA) Three-Address Code (TAC) form.
	 */
	struct Instruction
	{
		public:
		using Version   = u32;
		using Arguments = std::array<Instruction*, 2>;

		static constexpr Version k_invalid_version = std::numeric_limits<Version>::max();

		public:
		Version     version{ k_invalid_version };
		types::Type type{};
		Arguments   args{ Instruction::no_args() };

		public:
		constexpr Instruction(types::Type type, Arguments args);

		virtual ~Instruction() = default;

		constexpr bool operator==(const Instruction& other) const noexcept  = default;
		constexpr auto operator<=>(const Instruction& other) const noexcept = default;

		/** @brief Verifies if an Instruction is of a given type. */
		template <InstructionKind T>
		[[nodiscard]] constexpr bool is() const noexcept;

		/**
		 * @brief Returns the requested underlying type.
		 * @important Does not perform any validation - assumes that Instruction::is<T> was used first.
		 */
		template <InstructionKind T>
		[[nodiscard]] constexpr const T& as() const noexcept;

		/**
		 * @brief Returns the requested underlying type.
		 * @important Does not perform any validation - assumes that Instruction::is<T> was used first.
		 */
		template <InstructionKind T>
		[[nodiscard]] constexpr T& as() noexcept;

		protected:
		[[nodiscard]] static constexpr Arguments no_args() noexcept;
		[[nodiscard]] static constexpr Arguments single_arg(Instruction*) noexcept;
		[[nodiscard]] static constexpr Arguments two_args(Instruction*, Instruction*) noexcept;
	};

	/**
	 * @brief Unreachable instruction: causes an immediate trap signal; signals that the program is in an invalid state.
	 */
	struct Unreachable final : public Instruction
	{
		public:
		constexpr Unreachable();
		virtual ~Unreachable() override = default;

		constexpr bool operator==(const Unreachable& other) const noexcept  = default;
		constexpr auto operator<=>(const Unreachable& other) const noexcept = default;
	};

	/**
	 * @brief Noop instruction: no operation; does nothing.
	 */
	struct Noop final : public Instruction
	{
		public:
		constexpr Noop();
		virtual ~Noop() override = default;

		constexpr bool operator==(const Noop& other) const noexcept  = default;
		constexpr auto operator<=>(const Noop& other) const noexcept = default;
	};

	/**
	 * @brief Cast instruction: performs type conversion of a given expression.
	 */
	struct Cast final : public Instruction
	{
		public:
		constexpr Cast(types::Type type, Instruction* arg);
		virtual ~Cast() override = default;

		constexpr bool operator==(const Cast& other) const noexcept  = default;
		constexpr auto operator<=>(const Cast& other) const noexcept = default;
	};

	/**
	 * @brief Call instruction: performs function call with specified parameters.
	 */
	struct Call final : public Instruction
	{
		public:
		std::string               identifier;
		std::vector<Instruction*> parameters;

		public:
		constexpr Call(types::Type return_type, std::string identifier, std::vector<Instruction*> parameters);
		virtual ~Call() override = default;

		constexpr bool operator==(const Call& other) const noexcept  = default;
		constexpr auto operator<=>(const Call& other) const noexcept = default;
	};

	/**
	 * @brief Const instruction: holds an (immediate) value of a given type.
	 */
	struct Const final : public Instruction
	{
		public:
		Value value;

		public:
		constexpr Const(types::Type type, Value value);
		virtual ~Const() override = default;

		constexpr bool operator==(const Const& other) const noexcept  = default;
		auto           operator<=>(const Const& other) const noexcept = default;
	};

	/**
	 * @brief Jump instruction: performs an unconditional jump to a given target (BasicBlock).
	 */
	struct Jump final : public Instruction
	{
		public:
		BasicBlock* target;

		public:
		constexpr Jump(BasicBlock* target);
		virtual ~Jump() override = default;

		constexpr bool operator==(const Jump& other) const noexcept  = default;
		constexpr auto operator<=>(const Jump& other) const noexcept = default;
	};

	/**
	 * @brief JumpIf instruction: performs a conditional jump to either of two targets (BasicBlocks).
	 */
	struct JumpIf final : public Instruction
	{
		public:
		BasicBlock* then_block;
		BasicBlock* else_block;

		public:
		// NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
		constexpr JumpIf(Instruction* condition, BasicBlock* then_block, BasicBlock* else_block);

		constexpr bool operator==(const JumpIf& other) const noexcept  = default;
		constexpr auto operator<=>(const JumpIf& other) const noexcept = default;
	};

	/**
	 * @brief Return instruction: terminates control flow of a function and (possibly) returns an expression.
	 */
	struct Return final : public Instruction
	{
		public:
		constexpr Return(Instruction* arg);

		constexpr bool operator==(const Return& other) const noexcept  = default;
		constexpr auto operator<=>(const Return& other) const noexcept = default;
	};

	/**
	 * @brief StackStore instruction: represents a reserved scratch place (of a given type) on the stack for
	 * the function to use.
	 */
	struct StackSlot final : public Instruction
	{
		public:
		constexpr StackSlot(types::Type type);

		constexpr bool operator==(const StackSlot& other) const noexcept  = default;
		constexpr auto operator<=>(const StackSlot& other) const noexcept = default;
	};

	/**
	 * @brief StackStore instruction: stores the result of an expression into a given stack slot.
	 */
	struct StackStore final : public Instruction
	{
		public:
		StackSlot* slot;

		public:
		constexpr StackStore(StackSlot* slot, Instruction* value);

		constexpr bool operator==(const StackStore& other) const noexcept  = default;
		constexpr auto operator<=>(const StackStore& other) const noexcept = default;
	};

	/**
	 * @brief StackLoad instruction: reads the value currently stored in a given stack slot.
	 */
	struct StackLoad final : public Instruction
	{
		public:
		StackSlot* slot;

		public:
		constexpr StackLoad(StackSlot* slot);

		constexpr bool operator==(const StackLoad& other) const noexcept  = default;
		constexpr auto operator<=>(const StackLoad& other) const noexcept = default;
	};

	/**
	 * @brief Phi instruction: performs a read operation and returns the current value of its 'shadow variable', i.e.
	 * reads the current value that was set by its correlated Upsilon instruction.
	 * @see https://gist.github.com/pizlonator/cf1e72b8600b1437dda8153ea3fdb963
	 */
	struct Phi final : public Instruction
	{
		public:
		constexpr Phi(types::Type type);

		constexpr bool operator==(const Phi& other) const noexcept  = default;
		constexpr auto operator<=>(const Phi& other) const noexcept = default;
	};

	/**
	 * @brief Upsilon instruction: performs a write operation (stores a value) into a 'shadow variable' of a given
	 * phi node, i.e. stores a value that will be used by a correlated Phi instruction(s).
	 * @see https://gist.github.com/pizlonator/cf1e72b8600b1437dda8153ea3fdb963
	 */
	struct Upsilon final : public Instruction
	{
		public:
		Instruction* phi;

		public:
		// NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
		constexpr Upsilon(Instruction* value, Instruction* phi = nullptr);

		constexpr bool operator==(const Upsilon& other) const noexcept  = default;
		constexpr auto operator<=>(const Upsilon& other) const noexcept = default;
	};

	/**
	 * @brief Not instruction: performs a negation of an input (boolean) expression.
	 */
	struct Not final : public Instruction
	{
		public:
		constexpr Not(Instruction* arg);
		virtual ~Not() override = default;

		constexpr bool operator==(const Not& other) const noexcept  = default;
		constexpr auto operator<=>(const Not& other) const noexcept = default;
	};

#define SOUL_INSTRUCTION(name)                                                  \
	struct name final : public Instruction                                      \
	{                                                                           \
		public:                                                                 \
		constexpr name(types::Type type, Instruction* arg0, Instruction* arg1)  \
			: Instruction(std::move(type), Instruction::two_args(arg0, arg1))   \
		{                                                                       \
		}                                                                       \
		virtual ~name() override                                     = default; \
		constexpr bool operator==(const name& other) const noexcept  = default; \
		constexpr auto operator<=>(const name& other) const noexcept = default; \
	};
	SOUL_ARITHMETIC_INSTRUCTIONS
#undef SOUL_INSTRUCTION

#define SOUL_INSTRUCTION(name)                                                                                   \
	struct name final : public Instruction                                                                       \
	{                                                                                                            \
		public:                                                                                                  \
		constexpr name(Instruction* arg0, Instruction* arg1)                                                     \
			: Instruction(types::Type{ types::PrimitiveType::Kind::Boolean }, Instruction::two_args(arg0, arg1)) \
		{                                                                                                        \
		}                                                                                                        \
		virtual ~name() override                                     = default;                                  \
		constexpr bool operator==(const name& other) const noexcept  = default;                                  \
		constexpr auto operator<=>(const name& other) const noexcept = default;                                  \
	};
	SOUL_COMPARISON_INSTRUCTIONS
	SOUL_LOGICAL_INSTRUCTIONS
#undef SOUL_INSTRUCTION
}  // namespace soul::ir
#include "ir/instruction.inl"
