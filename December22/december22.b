MODULO = 16777216

next_secret: function(secret) {
    * Step 1: Multiply by 64 and mix
    secret = (secret ^ (secret * 64)) % MODULO;
    * Step 2: Divide by 32 and mix
    secret = (secret ^ (secret / 32)) % MODULO;
    * Step 3: Multiply by 2048 and mix
    secret = (secret ^ (secret * 2048)) % MODULO;
    return secret;
}

generate_2000th_secret: function(initial_secret) {
    auto initial_secret, secret, i;
    secret = initial_secret;
    for (i = 0; i < 2000; i = i + 1) {
        secret = next_secret(secret);
    }
    return secret;
}

main: {
    auto file, initial_secrets[100], num_buyers, i, secret, sum;
    file = fopen("input.txt", "r");
    if (file == 0) {
        printf("Error opening file\n");
        return 1;
    }

    num_buyers = 0;
    while (fscanf(file, "%u", &initial_secrets[num_buyers]) != EOF) {
        num_buyers = num_buyers + 1;
    }
    fclose(file);

    sum = 0;
    for (i = 0; i < num_buyers; i = i + 1) {
        secret = generate_2000th_secret(initial_secrets[i]);
        printf("Buyer %d: %u\n", i + 1, secret);
        sum = sum + secret;
    }

    printf("Sum of 2000th secret numbers: %u\n", sum);
    return 0;
}